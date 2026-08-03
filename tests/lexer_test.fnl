;; formula tokens: multi-word idents, $refs$, Table.Col

(local {: suite : assert-eq : assert-error} (require :test_util))
(local {: lex} (require :lexer))

(fn t [type value]
  (if (= type :lparen) {:type :lparen}
      (= type :rparen) {:type :rparen}
      (= type :num) {:type :num :value value}
      (= type :ident) {:type :ident :value value}
      (= type :op) {:type :op :value value}
      (= type :varref) {:type :varref :value value}
      (error (.. "bad token shorthand: " (tostring type)))))

(fn dotref [tbl col]
  {:type :dotref :table tbl :col col})

(fn run []
  (suite "simple arithmetic idents")
  (assert-eq (lex "Damage / Firerate")
             [(t :ident "Damage") (t :op "/") (t :ident "Firerate")]
             "Damage / Firerate")
  (suite "multi-word ident + varref")
  (assert-eq (lex "Total Price / $MDPS$")
             [(t :ident "Total Price") (t :op "/") (t :varref "MDPS")]
             "Total Price / $MDPS$")
  (suite "multi-word ident + ops + number")
  (assert-eq (lex "Total Price / DPS * 2")
             [(t :ident "Total Price")
              (t :op "/")
              (t :ident "DPS")
              (t :op "*")
              (t :num 2)] "Total Price / DPS * 2")
  (suite "dotref + nested parens + pow + unary minus")
  (assert-eq (lex "Firework Technician Stats.Firework Shot Chance Cap / (1 + 2.718**(-0.5*(Level)))")
             [(dotref "Firework Technician Stats" "Firework Shot Chance Cap")
              (t :op "/")
              (t :lparen)
              (t :num 1)
              (t :op "+")
              (t :num 2.718)
              (t :op "**")
              (t :lparen)
              (t :op "-")
              (t :num 0.5)
              (t :op "*")
              (t :lparen)
              (t :ident "Level")
              (t :rparen)
              (t :rparen)
              (t :rparen)]
             "Firework Technician Stats... exponential formula")
  (suite "all operators")
  (assert-eq (lex "a + b - c * d / e % f ** g")
             [(t :ident "a")
              (t :op "+")
              (t :ident "b")
              (t :op "-")
              (t :ident "c")
              (t :op "*")
              (t :ident "d")
              (t :op "/")
              (t :ident "e")
              (t :op "%")
              (t :ident "f")
              (t :op "**")
              (t :ident "g")] "operator set")
  (suite "numbers: decimals, leading dot, thousands separators")
  (assert-eq (lex "18275 / 36 * 6")
             [(t :num 18275) (t :op "/") (t :num 36) (t :op "*") (t :num 6)]
             "integer arithmetic")
  (assert-eq (lex ".5 + 1.25") [(t :num 0.5) (t :op "+") (t :num 1.25)]
             "leading-dot decimal")
  (assert-eq (lex "18,275 + 1_000") [(t :num 18275) (t :op "+") (t :num 1000)]
             "comma/underscore separators")
  (suite "varref forms (pin / FNC kept as raw value for parser)")
  (assert-eq (lex "$MCE$ + $TOKEN@5@Branch$ + $FNC-TOTALPRICE$")
             [(t :varref "MCE")
              (t :op "+")
              (t :varref "TOKEN@5@Branch")
              (t :op "+")
              (t :varref "FNC-TOTALPRICE")] "varref variants")
  (suite "trailing digits stay in ident (DPS2)")
  (assert-eq (lex "DPS2 + x") [(t :ident "DPS2") (t :op "+") (t :ident "x")]
             "DPS2 is one ident")
  (suite "glued hyphen in bare ident (Charge-Up)")
  (assert-eq (lex "Overcharge / (Charge-Up + Cooldown)")
             [(t :ident "Overcharge")
              (t :op "/")
              (t :lparen)
              (t :ident "Charge-Up")
              (t :op "+")
              (t :ident "Cooldown")
              (t :rparen)] "Charge-Up")
  (suite "binary minus keeps spaces (not a hyphenated name)")
  (assert-eq (lex "Whirlwind Hit - 1")
             [(t :ident "Whirlwind Hit") (t :op "-") (t :num 1)]
             "space-minus is binary")
  (suite "postfix -- and ++")
  (assert-eq (lex "(Cash Shot--)")
             [(t :lparen) (t :ident "Cash Shot") (t :op "--") (t :rparen)]
             "Cash Shot--")
  (assert-eq (lex "Burst Size++") [(t :ident "Burst Size") (t :op "++")]
             "Burst Size++")
  (suite "[[wikilink]] is column ident")
  (assert-eq (lex "[[Splash Damage]] / Firerate")
             [(t :ident "Splash Damage") (t :op "/") (t :ident "Firerate")]
             "wikilink -> Splash Damage")
  (assert-eq (lex "Cannon [[Splash Damage]] / Cannon Firerate")
             [(t :ident "Cannon Splash Damage")
              (t :op "/")
              (t :ident "Cannon Firerate")]
             "embedded wikilink folds into multi-word ident")
  (suite "whitespace is insignificant between tokens")
  (assert-eq (lex "  a   +   1  ") [(t :ident "a") (t :op "+") (t :num 1)]
             "extra spaces")
  (suite "empty / whitespace-only")
  (assert-eq (lex "") [] "empty string")
  (assert-eq (lex "   \t  ") [] "whitespace only")
  (suite "errors")
  (assert-error #(lex "$unclosed") "unclosed")
  (assert-error #(lex "$$") "empty")
  true)

{:run run}
