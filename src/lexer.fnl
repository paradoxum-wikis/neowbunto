;; Formula tokenizer

(fn at [s i]
	(s:sub i i))

(fn trim [str]
	(pick-values 1 (str:match "^%s*(.-)%s*$")))

(fn starts-number? [s i]
	(let [c (at s i)]
		(or (c:match "%d")
				(and (= c ".")
						 (let [n (at s (+ i 1))]
							 (and n (n:match "%d")))))))

(fn is-op-char? [c]
	(or (= c "+") (= c "-") (= c "*") (= c "/") (= c "%")))

(fn skip-ws [s i]
	(var j i)
	(while (let [c (at s j)]
					 (and (not= c "") (c:match "%s")))
		(set j (+ j 1)))
	j)

(fn read-number [s i]
	;; slice once at end
	;; no per-char string concat
	(var j i)
	(var saw-dot false)
	(when (and (= (at s j) ".")
						 (let [n (at s (+ j 1))]
							 (and n (n:match "%d"))))
		(set saw-dot true)
		(set j (+ j 1)))
	(var done false)
	(while (not done)
		(let [c (at s j)]
			(if (or (c:match "%d") (= c ",") (= c "_"))
					(set j (+ j 1))
					(and (= c ".")
							 (not saw-dot)
							 (let [n (at s (+ j 1))]
								 (and n (n:match "%d"))))
					(do
						(set saw-dot true)
						(set j (+ j 1)))
					(set done true))))
	(let [raw (s:sub i (- j 1))
				cleaned (pick-values 1 (raw:gsub "[,_]" ""))
				n (tonumber cleaned)]
		(when (not n)
			(error (.. "lexer: invalid number `" raw "`")))
		(values {:type :num :value n} j)))

(fn read-varref [s i]
	(let [close (s:find "%$" (+ i 1))]
		(when (not close)
			(error (.. "lexer: unclosed $ starting at position " (tostring i))))
		(let [inner (s:sub (+ i 1) (- close 1))]
			(when (= inner "")
				(error (.. "lexer: empty $...$ at position " (tostring i))))
			(values {:type :varref :value inner} (+ close 1)))))

(fn read-wikilink-span [s i]
	(when (not= (at s (+ i 1)) "[")
		(error (.. "lexer: expected [[ at position " (tostring i))))
	(let [close (s:find "%]%]" (+ i 2))]
		(when (not close)
			(error (.. "lexer: unclosed [[ starting at position " (tostring i))))
		(let [inner (trim (s:sub (+ i 2) (- close 1)))
					name (or (inner:match "^([^|]+)") inner)
					name (trim name)]
			(when (= name "")
				(error (.. "lexer: empty [[...]] at position " (tostring i))))
			(values name (+ close 2)))))

(fn read-wikilink [s i]
	;; i.e. pure [[...]] is a column name
	(let [(name ni) (read-wikilink-span s i)]
		(values {:type :ident :value name} ni)))

(fn col-start? [s j]
	(and (= (at s j) ".")
			 (let [n (at s (+ j 1))]
				 (and (not= n "")
							(not (n:match "%d"))
							(not (n:match "%s"))
							(not (is-op-char? n))
							(not= n "(") (not= n ")") (not= n "$")
							(not= n ".") (not= n "[")))))

(fn glued-hyphen? [s j]
	;; i.e. Charge-Up alnum - alnum
	;; not binary 'a - b' and not postfix --
	(and (= (at s j) "-")
			 (not= (at s (+ j 1)) "-")
			 (let [prev (at s (- j 1))
						 nxt (at s (+ j 1))]
				 (and prev (prev:match "[%w]")
							nxt (nxt:match "[%w]")))))

(fn word-char? [s j in-col?]
	(let [c (at s j)]
		(if (= c "")
				false
				(= c "-")
				(or in-col? (glued-hyphen? s j))
				(or (= c "(") (= c ")") (= c "$") (= c "[") (= c "]")
						(is-op-char? c))
				false
				;; letters, digits, underscore, spaces (multi-word)
				true)))

(fn read-wordish [s i]
	(var j i)
	(var dot-at nil)
	(var done false)
	(local pieces [])
	(var piece-start i)
	(while (and (not done) (<= j (length s)))
		(let [c (at s j)]
			(if (and (= c "[") (= (at s (+ j 1)) "["))
					(do
						(when (< piece-start j)
							(table.insert pieces (s:sub piece-start (- j 1))))
						(let [(inner nj) (read-wikilink-span s j)]
							(table.insert pieces inner)
							(set j nj)
							(set piece-start j)))
					(col-start? s j)
					(if dot-at
							(set done true)
							(do
								(set dot-at j)
								(set j (+ j 1))))
					(word-char? s j (not= dot-at nil))
					(set j (+ j 1))
					(set done true))))
	(when (< piece-start j)
		(table.insert pieces (s:sub piece-start (- j 1))))
	(if dot-at
			;; concat pieces so embedded [[...]] is already stripped before the .
			(let [full (trim (table.concat pieces))
						dot-pos (full:find "." 1 true)]
				(if (not dot-pos)
						(error (.. "lexer: malformed table.col near position " (tostring i)))
						(let [tbl (trim (full:sub 1 (- dot-pos 1)))
									col (trim (full:sub (+ dot-pos 1)))]
							(when (or (= tbl "") (= col ""))
								(error (.. "lexer: malformed table.col near position "
													 (tostring i))))
							(values {:type :dotref :table tbl :col col} j))))
			(let [name (trim (table.concat pieces))]
				(when (= name "")
					(error (.. "lexer: empty identifier at position " (tostring i))))
				(values {:type :ident :value name} j))))

(fn postfix-ok? [tokens]
	;; Cash Shot-- after a value
	;; --3 at start stays double unary minus
	(when (> (length tokens) 0)
		(let [lt (. tokens (length tokens) :type)]
			(or (= lt :ident) (= lt :rparen) (= lt :varref) (= lt :num)
					(= lt :dotref)))))

(fn lex [s]
	(when (not= (type s) :string)
		(error "lexer: expected a string"))
	(let [tokens []
				len (length s)]
		(var i 1)
		(while (<= i len)
			(set i (skip-ws s i))
			(when (<= i len)
				(let [c (at s i)]
					(if (= c "(")
							(do
								(table.insert tokens {:type :lparen})
								(set i (+ i 1)))
							(= c ")")
							(do
								(table.insert tokens {:type :rparen})
								(set i (+ i 1)))
							(= c "$")
							(let [(tok ni) (read-varref s i)]
								(table.insert tokens tok)
								(set i ni))
							(and (= c "[") (= (at s (+ i 1)) "["))
							(let [(tok ni) (read-wikilink s i)]
								(table.insert tokens tok)
								(set i ni))
							(starts-number? s i)
							(let [(tok ni) (read-number s i)]
								(table.insert tokens tok)
								(set i ni))
							(and (= c "*") (= (at s (+ i 1)) "*"))
							(do
								(table.insert tokens {:type :op :value "**"})
								(set i (+ i 2)))
							;; postfix ++ / -- only after a primary
							(and (= c "+") (= (at s (+ i 1)) "+") (postfix-ok? tokens))
							(do
								(table.insert tokens {:type :op :value "++"})
								(set i (+ i 2)))
							(and (= c "-") (= (at s (+ i 1)) "-") (postfix-ok? tokens))
							(do
								(table.insert tokens {:type :op :value "--"})
								(set i (+ i 2)))
							(is-op-char? c)
							(do
								(table.insert tokens {:type :op :value c})
								(set i (+ i 1)))
							(let [(tok ni) (read-wordish s i)]
								(table.insert tokens tok)
								(set i ni))))))
		tokens))

{:lex lex}
