;; Formula tokenizer
;; no lookup tables, the chunk reruns per #invoke

(local byte string.byte)
(local find string.find)
(local sub string.sub)
(local tinsert table.insert)

(fn trim [str]
  (str:match "^%s*(.-)%s*$"))

(fn digit? [b]
  (and b (<= 48 b 57)))

(fn alnum? [b]
  (and b (or (<= 48 b 57) (<= 65 b 90) (<= 97 b 122))))

(fn skip-ws [s i]
  (let [(_ e) (find s "^%s*" i)]
    (+ e 1)))

(fn read-number [s i]
  (let [lead (= (byte s i) 46)
        start (if lead (+ i 1) i)
        (_ e) (find s "^[%d,_]*" start)
        j (if (and (not lead) (= (byte s (+ e 1)) 46) (digit? (byte s (+ e 2))))
              (let [(_ e2) (find s "^[%d,_]*" (+ e 2))]
                (+ e2 1))
              (+ e 1))
        raw (sub s i (- j 1))
        n (or (tonumber raw) (tonumber (pick-values 1 (raw:gsub "[,_]" ""))))]
    (when (not n)
      (error (.. "lexer: invalid number `" raw "`")))
    (values {:type :num :value n} j)))

(fn read-varref [s i]
  (let [close (find s "$" (+ i 1) true)]
    (when (not close)
      (error (.. "lexer: unclosed $ starting at position " (tostring i))))
    (let [inner (sub s (+ i 1) (- close 1))]
      (when (= inner "")
        (error (.. "lexer: empty $...$ at position " (tostring i))))
      (values {:type :varref :value inner} (+ close 1)))))

(fn read-wikilink-span [s i]
  (let [close (find s "]]" (+ i 2) true)]
    (when (not close)
      (error (.. "lexer: unclosed [[ starting at position " (tostring i))))
    ;; [[|x]] keeps the pipe
    (let [inner (trim (sub s (+ i 2) (- close 1)))
          p (find inner "|" 1 true)
          cut (if (and p (> p 1)) (sub inner 1 (- p 1)) inner)
          name (trim cut)]
      (when (= name "")
        (error (.. "lexer: empty [[...]] at position " (tostring i))))
      (values name (+ close 2)))))

(fn read-wikilink [s i]
  ;; i.e. pure [[...]] is a column name
  (let [(name ni) (read-wikilink-span s i)]
    (values {:type :ident :value name} ni)))

(fn col-start? [s j]
  (and (byte s (+ j 1)) (not (find s "^[%s%d%+%-%*/%%%(%)%$%.%[]" (+ j 1)))))

(fn glued? [s j]
  ;; i.e. Charge-Up alnum - alnum
  ;; not binary 'a - b' and not postfix --
  (and (alnum? (byte s (- j 1))) (alnum? (byte s (+ j 1)))))

(fn read-wordish [s i]
  ;; other punct (_ , ' #) is part of the name too
  (var j i)
  (var dot nil)
  (var text nil)
  (var ps i)
  (var more true)
  (while more
    (let [(_ e) (find s "^%P*" j)]
      (set j (+ e 1))
      (let [b (byte s j)]
        (if (= b 46)
            (if (not (col-start? s j)) (set j (+ j 1))
                dot (set more false)
                (do
                  (set dot j)
                  (set j (+ j 1))))
            (= b 45)
            (if (or dot (glued? s j)) (set j (+ j 1)) (set more false))
            (and (= b 91) (= (byte s (+ j 1)) 91))
            (let [(name nj) (read-wikilink-span s j)]
              ;; concat pieces so embedded [[...]] is already stripped before the .
              (set text (.. (or text "") (sub s ps (- j 1)) name))
              (set j nj)
              (set ps nj))
            (or (not b) (= b 40) (= b 41) (= b 36) (= b 91) (= b 93) (= b 43)
                (= b 42) (= b 47) (= b 37))
            (set more false)
            (set j (+ j 1))))))
  ;; s[i] is never blank (skip-ws), so this stops at i
  (var k (- j 1))
  (while (let [c (byte s k)]
           (and c (or (= c 32) (<= 9 c 13))))
    (set k (- k 1)))
  (let [word (if text (trim (.. text (sub s ps (- j 1)))) (sub s i k))]
    (if dot
        ;; first dot in the text, not dot (A. B.C -> A / B.C)
        (let [dp (find word "." 1 true)
              tbl (trim (sub word 1 (- dp 1)))
              col (trim (sub word (+ dp 1)))]
          (when (or (= tbl "") (= col ""))
            (error (.. "lexer: malformed table.col near position " (tostring i))))
          (values {:type :dotref :table tbl :col col} j))
        (do
          (when (= word "")
            (error (.. "lexer: empty identifier at position " (tostring i))))
          (values {:type :ident :value word} j)))))

(fn postfix-ok? [tokens]
  ;; Cash Shot-- after a value
  ;; --3 at start stays double unary minus
  (let [t (. tokens (length tokens))]
    (and t (or (= t.type :ident) (= t.type :rparen) (= t.type :varref)
               (= t.type :num) (= t.type :dotref)))))

(fn read-token [s i tokens]
  (let [(b n) (byte s i (+ i 1))]
    (if (= b 40) (values {:type :lparen} (+ i 1)) (= b 41)
        (values {:type :rparen} (+ i 1)) (= b 36) (read-varref s i)
        (and (= b 91) (= n 91)) (read-wikilink s i)
        (or (<= 48 b 57) (and (= b 46) n (<= 48 n 57))) (read-number s i)
        (and (= b 42) (= n 42)) (values {:type :op :value "**"} (+ i 2))
        ;; postfix ++ / -- only after a primary
        (and (= b 43) (= n 43) (postfix-ok? tokens))
        (values {:type :op :value "++"} (+ i 2))
        (and (= b 45) (= n 45) (postfix-ok? tokens))
        (values {:type :op :value "--"} (+ i 2))
        (or (= b 43) (= b 45) (= b 42) (= b 47) (= b 37))
        (values {:type :op :value (sub s i i)} (+ i 1)) (read-wordish s i))))

(fn lex [s]
  (when (not= (type s) :string)
    (error "lexer: expected a string"))
  (let [tokens []
        len (length s)]
    (var i (skip-ws s 1))
    (while (<= i len)
      (let [(tok ni) (read-token s i tokens)]
        (tinsert tokens tok)
        (set i (skip-ws s ni))))
    tokens))

{:lex lex}
