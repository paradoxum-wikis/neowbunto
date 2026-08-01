;; local MediaWiki #expr parser func

(fn trim [s]
	(let [str (tostring s)]
		(pick-values 1 (str:match "^%s*(.-)%s*$"))))

(fn is-alpha [c]
	(and c (c:match "[%a_]")))

(fn is-alnum [c]
	(and c (c:match "[%w_]")))

(fn is-digit [c]
	(and c (c:match "%d")))

(fn tokenize [source]
	;; recursive descent
	;; no load() / string rewrite for tests without a Frame
	(let [s (tostring (or source ""))
				tokens []
				len (length s)]
		(var i 1)
		(while (<= i len)
			(let [c (s:sub i i)]
				(if (c:match "%s")
						(set i (+ i 1))
						(is-digit c)
						(do
							(var j i)
							(while (and (<= j len) (or (is-digit (s:sub j j))
																				 (= (s:sub j j) ".")))
								(set j (+ j 1)))
							(table.insert tokens {:type :num
																		:value (tonumber (s:sub i (- j 1)))})
							(set i j))
						(is-alpha c)
						(do
							(var j i)
							(while (and (<= j len) (is-alnum (s:sub j j)))
								(set j (+ j 1)))
							(let [word (s:sub i (- j 1))
										lower (word:lower)]
								(table.insert tokens
															(if (or (= lower "e") (= lower "pi")
																			(= lower "div") (= lower "mod")
																			(= lower "and") (= lower "or")
																			(= lower "not")
																			(= lower "abs") (= lower "floor")
																			(= lower "ceil") (= lower "trunc")
																			(= lower "round") (= lower "sqrt")
																			(= lower "ln") (= lower "exp")
																			(= lower "sin") (= lower "cos")
																			(= lower "tan") (= lower "asin")
																			(= lower "acos") (= lower "atan"))
																	{:type :word :value lower}
																	{:type :ident :value word}))
								(set i j)))
						;; multi-char operators
						(and (= c "<") (= (s:sub (+ i 1) (+ i 1)) "="))
						(do (table.insert tokens {:type :op :value "<="}) (set i (+ i 2)))
						(and (= c ">") (= (s:sub (+ i 1) (+ i 1)) "="))
						(do (table.insert tokens {:type :op :value ">="}) (set i (+ i 2)))
						(and (= c "<") (= (s:sub (+ i 1) (+ i 1)) ">"))
						(do (table.insert tokens {:type :op :value "<>"}) (set i (+ i 2)))
						(and (= c "!") (= (s:sub (+ i 1) (+ i 1)) "="))
						(do (table.insert tokens {:type :op :value "!="}) (set i (+ i 2)))
						(and (= c "=") (= (s:sub (+ i 1) (+ i 1)) "="))
						(do (table.insert tokens {:type :op :value "="}) (set i (+ i 2)))
						(= c "=")
						(do (table.insert tokens {:type :op :value "="}) (set i (+ i 1)))
						(or (= c "+") (= c "-") (= c "*") (= c "/") (= c "^")
								(= c "(") (= c ")") (= c "<") (= c ">"))
						(do
							(table.insert tokens {:type (if (or (= c "(") (= c ")"))
																							(if (= c "(") :lparen :rparen)
																							:op)
																		:value c})
							(set i (+ i 1)))
						(error (.. "mwexpr: unexpected character '" c
											 "' in #expr near position " (tostring i))))))
		(table.insert tokens {:type :eof})
		tokens))


(fn truthy [n]
	(and n (not= n 0)))

(fn bool01 [b]
	(if b 1 0))

(fn parse-eval [source bindings]
	(let [tokens (tokenize source)
				bindings (or bindings {})]
		(var pos 1)

		(fn peek []
			(. tokens pos))

		(fn at [typ val]
			(let [t (peek)]
				(and (= t.type typ) (or (= val nil) (= t.value val)))))

		(fn advance []
			(let [t (peek)]
				(set pos (+ pos 1))
				t))

		(fn expect [typ val]
			(let [t (peek)]
				(when (not (and (= t.type typ) (or (= val nil) (= t.value val))))
					(error (.. "mwexpr: expected " (tostring typ)
										 (if val (.. " " val) "")
										 ", got " (tostring t.type))))
				(advance)))

		(var parse-or nil)

		(fn parse-primary []
			(let [t (peek)]
				(if (= t.type :num)
						(do (advance) t.value)
						(and (= t.type :word) (= t.value "e"))
						(do (advance) (math.exp 1))
						(and (= t.type :word) (= t.value "pi"))
						(do (advance) math.pi)
						(and (= t.type :word)
								 (or (= t.value "abs") (= t.value "floor") (= t.value "ceil")
										 (= t.value "trunc") (= t.value "round") (= t.value "sqrt")
										 (= t.value "ln") (= t.value "exp") (= t.value "sin")
										 (= t.value "cos") (= t.value "tan") (= t.value "asin")
										 (= t.value "acos") (= t.value "atan")))
						(let [fname t.value]
							(advance)
							(expect :lparen)
							(let [arg (parse-or)]
								(expect :rparen)
								(case fname
									:abs (math.abs arg)
									:floor (math.floor arg)
									:ceil (math.ceil arg)
									:trunc (if (>= arg 0) (math.floor arg) (math.ceil arg))
									:round (math.floor (+ arg 0.5))
									:sqrt (math.sqrt arg)
									:ln (math.log arg)
									:exp (math.exp arg)
									:sin (math.sin arg)
									:cos (math.cos arg)
									:tan (math.tan arg)
									:asin (math.asin arg)
									:acos (math.acos arg)
									:atan (math.atan arg))))
						(= t.type :ident)
						(let [name t.value]
							(advance)
							(let [v (or (. bindings name)
													(. bindings (name:lower)))]
								(when (= v nil)
									(error (.. "mwexpr: unknown identifier '" name "'")))
								(tonumber v)))
						(= t.type :lparen)
						(do
							(advance)
							(let [v (parse-or)]
								(expect :rparen)
								v))
						(error (.. "mwexpr: unexpected token "
											 (tostring t.type) " "
											 (tostring t.value))))))

		(fn parse-unary []
			(if (at :op "+")
					(do (advance) (parse-unary))
					(at :op "-")
					(do (advance) (- (parse-unary)))
					(at :word "not")
					(do (advance) (bool01 (not (truthy (parse-unary)))))
					(parse-primary)))

		(fn parse-power []
			;; MediaWiki ^ is right assoc
			(let [base (parse-unary)]
				(if (at :op "^")
						(do (advance) (^ base (parse-power)))
						base)))

		(fn parse-product []
			(var v (parse-power))
			(while (or (at :op "*") (at :op "/")
								 (at :word "div") (at :word "mod"))
				(let [op (. (advance) :value)
							r (parse-power)]
					(set v (if (= op "*")
										 (* v r)
										 (or (= op "/") (= op "div"))
										 (if (= r 0)
												 (error "mwexpr: division by zero")
												 (/ v r))
										 (= op "mod")
										 (% v r)
										 v))))
			v)

		(fn mw-round [x digits]
			;; MediaWiki x round y -> round x to y decimal places
			(let [d (or digits 0)
						m (^ 10 d)]
				(/ (math.floor (+ (* x m) 0.5)) m)))

		(fn parse-round []
			;; infix round binds looser than * /  (a * b round 0)
			(var v (parse-product))
			(while (at :word "round")
				(advance)
				(set v (mw-round v (parse-product))))
			v)

		(fn parse-sum []
			(var v (parse-round))
			(while (or (at :op "+") (at :op "-"))
				(let [op (. (advance) :value)
							r (parse-round)]
					(set v (if (= op "+") (+ v r) (- v r)))))
			v)

		(fn parse-compare []
			(let [l (parse-sum)
						t (peek)]
				(if (and (= t.type :op)
								 (or (= t.value "=") (= t.value "<>") (= t.value "!=")
										 (= t.value "<") (= t.value ">")
										 (= t.value "<=") (= t.value ">=")))
						(let [op (. (advance) :value)
									r (parse-sum)]
							(bool01 (if (= op "=") (= l r)
													(or (= op "<>") (= op "!=")) (not= l r)
													(= op "<") (< l r)
													(= op ">") (> l r)
													(= op "<=") (<= l r)
													(= op ">=") (>= l r))))
						l)))

		(fn parse-and []
			(var v (parse-compare))
			(while (at :word "and")
				(advance)
				(let [r (parse-compare)]
					(set v (bool01 (and (truthy v) (truthy r))))))
			v)

		(set parse-or
				 (fn []
					 (var v (parse-and))
					 (while (at :word "or")
						 (advance)
						 (let [r (parse-and)]
							 (set v (bool01 (or (truthy v) (truthy r))))))
					 v))

		(let [result (parse-or)]
			(when (not= (. (peek) :type) :eof)
				(error (.. "mwexpr: trailing input near "
									 (tostring (. (peek) :value)))))
			result)))

(fn strip-expr-wrapper [raw]
	(let [s (trim (or raw ""))]
		(s:match "^{{#expr:(.*)}}%s*$")))

(fn eval [source bindings]
	(parse-eval source bindings))

(fn eval-wrapped [raw bindings]
	(let [body (or (strip-expr-wrapper raw) raw)]
		(eval body bindings)))

{:tokenize tokenize
 :eval eval
 :eval-wrapped eval-wrapped
 :strip-expr-wrapper strip-expr-wrapper
 :parse-eval parse-eval}
