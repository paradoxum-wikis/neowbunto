;; expression tree shapes + precedence

(local {: suite : assert-eq : assert-error} (require :test_util))
(local {: parse : parse-string} (require :parser))
(local {: lex} (require :lexer))

(fn run []
  (suite "18275/36*6 is left-associative (* of /)")
  (assert-eq (parse-string "18275/36*6")
             [:binop "*"
                     [:binop "/" [:num 18275] [:num 36]]
                     [:num 6]]
             "18275/36*6")

  (suite "2+3*4: * binds tighter than +")
  (assert-eq (parse-string "2+3*4")
             [:binop "+"
                     [:num 2]
                     [:binop "*" [:num 3] [:num 4]]]
             "2+3*4")

  ;; ** is right-assoc: 2**3**2 = 2**(3**2) = 512 not 64
  (suite "** is right-associative (matches old calc)")
  (assert-eq (parse-string "2**3**2")
             [:pow [:num 2] [:pow [:num 3] [:num 2]]]
             "2**3**2 => 2**(3**2)")
  (assert-eq (parse-string "(2**3)**2")
             [:pow [:pow [:num 2] [:num 3]] [:num 2]]
             "(2**3)**2 left-grouped")
  (assert-eq (parse-string "2**(3**2)")
             [:pow [:num 2] [:pow [:num 3] [:num 2]]]
             "2**(3**2) explicit right")

  (suite "+ - left-associative")
  (assert-eq (parse-string "1-2-3")
             [:binop "-"
                     [:binop "-" [:num 1] [:num 2]]
                     [:num 3]]
             "1-2-3")
  (assert-eq (parse-string "1+2+3")
             [:binop "+"
                     [:binop "+" [:num 1] [:num 2]]
                     [:num 3]]
             "1+2+3")

  (suite "* / % left-associative")
  (assert-eq (parse-string "a/b/c")
             [:binop "/"
                     [:binop "/" [:ident "a"] [:ident "b"]]
                     [:ident "c"]]
             "a/b/c")
  (assert-eq (parse-string "a*b%c")
             [:binop "%"
                     [:binop "*" [:ident "a"] [:ident "b"]]
                     [:ident "c"]]
             "a*b%c")

  (suite "mixed precedence + * **")
  (assert-eq (parse-string "1+2*3**2")
             [:binop "+"
                     [:num 1]
                     [:binop "*"
                             [:num 2]
                             [:pow [:num 3] [:num 2]]]]
             "1+2*3**2")

  (suite "parens override precedence")
  (assert-eq (parse-string "(1+2)*3")
             [:binop "*"
                     [:binop "+" [:num 1] [:num 2]]
                     [:num 3]]
             "(1+2)*3")
  (assert-eq (parse-string "((2))")
             [:num 2]
             "nested parens collapse")

  (suite "unary minus binds tighter than **")
  (assert-eq (parse-string "-2**2")
             [:pow [:unop "-" [:num 2]] [:num 2]]
             "-2**2 => (-2)**2")
  ;; unary binds the primary; * is binary after that
  (assert-eq (parse-string "2.718**(-0.5*(Level))")
             [:pow [:num 2.718]
                   [:binop "*"
                           [:unop "-" [:num 0.5]]
                           [:ident "Level"]]]
             "2.718**(-0.5*(Level))")
  (assert-eq (parse-string "--3")
             [:unop "-" [:unop "-" [:num 3]]]
             "double unary")
  (assert-eq (parse-string "Cash Shot--")
             [:binop "-" [:ident "Cash Shot"] [:num 1]]
             "postfix -- -> − 1")
  (assert-eq (parse-string "(Burst Size--) * Firerate")
             [:binop "*"
                     [:binop "-" [:ident "Burst Size"] [:num 1]]
                     [:ident "Firerate"]]
             "Burst Size-- in parens")

  (suite "multi-word ident + varref leaf (no env = no splice)")
  (assert-eq (parse-string "Total Price / $MDPS$")
             [:binop "/"
                     [:ident "Total Price"]
                     [:varref "MDPS"]]
             "Total Price / $MDPS$")

  (suite "dotref primary")
  (assert-eq (parse-string "Firework Technician Stats.Firework Shot Chance Cap / 2")
             [:binop "/"
                     [:dotref "Firework Technician Stats"
                              "Firework Shot Chance Cap"]
                     [:num 2]]
             "dotref / 2")

  (suite "Damage / Firerate")
  (assert-eq (parse-string "Damage / Firerate")
             [:binop "/" [:ident "Damage"] [:ident "Firerate"]]
             "Damage / Firerate")

  (suite "parse accepts token list from lex")
  (assert-eq (parse (lex "3 * 4 + 1"))
             [:binop "+"
                     [:binop "*" [:num 3] [:num 4]]
                     [:num 1]]
             "lex then parse")

  (suite "parse errors")
  (assert-error #(parse-string "(1+2") "closing")
  (assert-error #(parse-string "1+") "end of input")
  (assert-error #(parse-string "* 2") "unexpected")
  (assert-error #(parse-string "") "empty")
  (assert-error #(parse []) "empty")
  (assert-error #(parse-string "1)") "unexpected")

  true)

{:run run}
