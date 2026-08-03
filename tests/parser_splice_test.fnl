;; $VAR$ text-macro + cycles

(local {: suite : assert-eq : assert-error : assert-true} (require :test_util))
(local {: parse-with-env : parse-var : parse-var-env : literal-raw?}
       (require :parser))

(fn run []
  (suite "bare $MDPS$ is text-macro left-assoc without auto-parens")
  (let [env {"MDPS" "DPS * Max Hits" "MCE" "Total Price / $MDPS$"}
        (ast cache) (parse-with-env (. env "MCE") env)]
    (assert-eq ast [:binop
                    "*"
                    [:binop "/" [:ident "Total Price"] [:ident "DPS"]]
                    [:ident "Max Hits"]]
               "Total Price / $MDPS$ -> (TP / DPS) * Max Hits")
    (assert-eq (. cache "MCE") nil
               "MCE not auto-cached by parse-with-env of body only")
    (assert-eq (parse-var "MDPS" env cache [])
               [:binop "*" [:ident "DPS"] [:ident "Max Hits"]] "MDPS parse-var"))
  (suite "parenthesized ($MDPS$) groups the pasted body")
  (let [env {"MDPS" "DPS * Max Hits" "MCE" "Total Price / ($MDPS$)"}
        (ast _) (parse-with-env (. env "MCE") env)]
    (assert-eq ast
               [:binop
                "/"
                [:ident "Total Price"]
                [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]]
               "Total Price / ($MDPS$)"))
  (suite "bare and parenthesized $MDPS$ differ")
  (let [env {"MDPS" "DPS * Max Hits"}
        (a _) (parse-with-env "Total Price / $MDPS$" env)
        (b _) (parse-with-env "Total Price / ($MDPS$)" env)]
    (assert-eq a [:binop
                  "*"
                  [:binop "/" [:ident "Total Price"] [:ident "DPS"]]
                  [:ident "Max Hits"]] "bare")
    (assert-eq b
               [:binop
                "/"
                [:ident "Total Price"]
                [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]]
               "paren")
    (assert-true (not= (. a 1) nil) "a ok")
    (assert-true (not= (tostring a) (tostring b)) "trees differ"))
  ;; Gladiator/Electroshocker: MCE reads the Max DPS column, not $MDPS$ paste
  (suite "production MCE is Total Price / Max DPS column")
  (let [env {"MDPS" "DPS * Max Hits" "MCE" "Total Price / Max DPS"}
        (ast _) (parse-with-env (. env "MCE") env)]
    (assert-eq ast [:binop "/" [:ident "Total Price"] [:ident "Max DPS"]]
               "column ident, no paste"))
  (suite "alias chain Hello -> World pastes to soft-ident")
  (let [env {"Hello" "World" "Alias" "$Hello$"}
        (ast _) (parse-with-env "$Alias$" env)]
    (assert-eq ast [:soft-ident "World"] "Alias resolves through Hello"))
  (suite "nested formula paste has no auto parens")
  (let [env {"A" "1 + 2" "B" "3 * $A$"}
        (ast _) (parse-with-env (. env "B") env)]
    (assert-eq ast [:binop "+" [:binop "*" [:num 3] [:num 1]] [:num 2]]
               "3 * $A$ with A = 1 + 2 -> 3 * 1 + 2"))
  (suite "cycle A -> B -> A raises naming both")
  (assert-error (fn []
                  (let [env {"A" "$B$ + 1" "B" "$A$ * 2"}]
                    (parse-with-env (. env "A") env))) "cyclic")
  (suite "cycle message names A and B")
  (let [env {"A" "$B$ + 1" "B" "$A$ * 2"}
        (ok err) (pcall parse-with-env (. env "A") env)]
    (assert-true (not ok) "should fail")
    (assert-true (: (tostring err) :find "A" 1 true) "mentions A")
    (assert-true (: (tostring err) :find "B" 1 true) "mentions B"))
  (suite "self-cycle $A$ = $A$ + 1")
  (assert-error (fn []
                  (parse-with-env "$A$ + 1" {"A" "$A$ + 1"}))
                "cyclic")
  (suite "undefined variable is a hard error")
  (assert-error (fn []
                  (parse-with-env "Total Price / $NOPE$" {}))
                "undefined variable")
  (suite "literal classification: pure <ref> / pure [[...]] only")
  (assert-true (literal-raw? "<ref>note</ref>") "ref tag")
  (assert-true (literal-raw? "[[Splash Damage]]") "pure wikilink")
  (assert-true (not (literal-raw? "[[Splash Damage]] / Firerate"))
               "wikilink in formula is arithmetic")
  (assert-true (not (literal-raw? "DPS * Max Hits")) "plain formula")
  (let [env {"DREF" "<ref name=\"x\">source</ref>"
             "SDPS" "[[Splash Damage]] / Firerate"}
        (ast _) (parse-with-env "$DREF$" env)
        (ast2 _) (parse-with-env "$SDPS$" env)]
    (assert-eq ast [:literal "<ref name=\"x\">source</ref>"]
               "ref var is literal, not parsed")
    (assert-eq ast2 [:binop "/" [:ident "Splash Damage"] [:ident "Firerate"]]
               "[[Splash Damage]] / Firerate"))
  (suite "malformed formula is a hard error (no silent fallback)")
  (assert-error (fn []
                  (parse-with-env "1 +" {})) "end of input")
  (suite "pin forms wrap expanded formula AST")
  (let [env {"MDPS" "DPS * Max Hits"}
        (ast _) (parse-with-env "$MDPS@5$" env)]
    (assert-eq ast [:pin 5 nil [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]]
               "$MDPS@5$")
    (let [(ast2 _) (parse-with-env "$MDPS@3@Top Path$" env)]
      (assert-eq ast2
                 [:pin
                  3
                  "Top Path"
                  [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]]
                 "$MDPS@3@Top Path$")))
  (suite "FNC-TOTALPRICE becomes intrinsic leaf")
  (let [(ast _) (parse-with-env "$FNC-TOTALPRICE$ / DPS" {})]
    (assert-eq ast [:binop "/" [:intrinsic :totalprice] [:ident "DPS"]]
               "intrinsic totalprice"))
  (assert-eq (parse-var "FNC-TOTALPRICE" {} {} []) [:intrinsic :totalprice]
             "top-level FNC-TOTALPRICE")
  (suite "FNC-TOTAL-EXP becomes intrinsic total")
  (let [(ast _) (parse-with-env "$FNC-TOTAL-EXP$" {})]
    (assert-eq ast [:intrinsic :total "EXP"] "TOTAL-EXP"))
  (suite "parse-var-env memoizes all user vars")
  (let [env {"MDPS" "DPS * Max Hits"
             "MCE" "Total Price / Max DPS"
             "FNC-COST" "100; 200; 300"}
        cache (parse-var-env env)]
    (assert-eq (. cache "MDPS") [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]
               "MDPS cached")
    (assert-eq (. cache "MCE")
               [:binop "/" [:ident "Total Price"] [:ident "Max DPS"]]
               "MCE cached as Max DPS column")
    (assert-eq (. cache "FNC-COST") nil "config key skipped"))
  (suite "repeated $X$ after paste is duplicated text (not shared AST)")
  (let [env {"X" "1 + 2" "Y" "$X$ * $X$"}
        cache (parse-var-env env)
        y (. cache "Y")]
    ;; 1 + 2 * 1 + 2 -> 1 + (2 * 1) + 2
    (assert-eq y [:binop
                  "+"
                  [:binop "+" [:num 1] [:binop "*" [:num 2] [:num 1]]]
                  [:num 2]] "Y = 1 + 2 * 1 + 2"))
  true)

{:run run}
