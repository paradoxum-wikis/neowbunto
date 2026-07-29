;; $VAR$ splice + cycles
;; MCE = Total Price / $MDPS$ must be TP / (DPS * Max Hits) not (TP / DPS) * Max Hits

(local {: suite : assert-eq : assert-error : assert-true} (require :test_util))
(local {: parse-with-env : parse-var : parse-var-env : parse-formula
        : literal-raw?}
       (require :parser))

(fn run []
  (suite "MCE splices MDPS as a real subtree (unparenthesized form)")
  (let [env {"MDPS" "DPS * Max Hits"
             "MCE" "Total Price / $MDPS$"}
        (ast cache) (parse-with-env (. env "MCE") env)]
    (assert-eq ast
               [:binop "/"
                       [:ident "Total Price"]
                       [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]]
               "Total Price / $MDPS$ with MDPS = DPS * Max Hits")
    (assert-eq (. cache "MDPS")
               [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]
               "MDPS memoized in parse-cache")
    (assert-eq (. cache "MCE") nil
               "MCE itself not auto-cached by parse-with-env of its body only"))

  (suite "MCE with parenthesized $MDPS$ yields the same tree shape")
  (let [env {"MDPS" "DPS * Max Hits"
             "MCE" "Total Price / ($MDPS$)"}
        (ast _) (parse-with-env (. env "MCE") env)]
    (assert-eq ast
               [:binop "/"
                       [:ident "Total Price"]
                       [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]]
               "Total Price / ($MDPS$) same structural grouping"))

  (suite "both MCE forms are structurally identical (the fix)")
  (let [env {"MDPS" "DPS * Max Hits"}
        (a _) (parse-with-env "Total Price / $MDPS$" env)
        (b _) (parse-with-env "Total Price / ($MDPS$)" env)]
    (assert-eq a b "parenthesized and bare splice agree"))

  (suite "Electroshocker-style MDPS splice into CE")
  (let [env {"MDPS" "DPS * Max Hits"
             "CE" "Total Price / $MDPS$"}
        (ast _) (parse-with-env (. env "CE") env)]
    (assert-eq ast
               [:binop "/"
                       [:ident "Total Price"]
                       [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]]
               "CE via MDPS"))

  (suite "alias chain Hello -> World splices to ident")
  (let [env {"Hello" "World"
             "Alias" "$Hello$"}
        (ast _) (parse-with-env "$Alias$" env)]
    (assert-eq ast [:ident "World"] "Alias resolves through Hello"))

  (suite "nested formula splice keeps operator structure")
  (let [env {"A" "1 + 2"
             "B" "3 * $A$"}
        (ast _) (parse-with-env (. env "B") env)]
    (assert-eq ast
               [:binop "*"
                       [:num 3]
                       [:binop "+" [:num 1] [:num 2]]]
               "3 * $A$ with A = 1 + 2"))

  (suite "cycle A -> B -> A raises naming both")
  (assert-error
    (fn []
      (let [env {"A" "$B$ + 1"
                 "B" "$A$ * 2"}]
        (parse-with-env (. env "A") env)))
    "cyclic")

  (suite "cycle message names A and B")
  (let [env {"A" "$B$ + 1"
             "B" "$A$ * 2"}
        (ok err) (pcall parse-with-env (. env "A") env)]
    (assert-true (not ok) "should fail")
    (assert-true (: (tostring err) :find "A" 1 true) "mentions A")
    (assert-true (: (tostring err) :find "B" 1 true) "mentions B"))

  (suite "self-cycle $A$ = $A$ + 1")
  (assert-error
    (fn []
      (parse-with-env "$A$ + 1" {"A" "$A$ + 1"}))
    "cyclic")

  (suite "undefined variable is a hard error")
  (assert-error
    (fn []
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
    (assert-eq ast
               [:literal "<ref name=\"x\">source</ref>"]
               "ref var is literal, not parsed")
    (assert-eq ast2
               [:binop "/"
                       [:ident "Splash Damage"]
                       [:ident "Firerate"]]
               "[[Splash Damage]] / Firerate"))

  (suite "malformed formula is a hard error (no silent fallback)")
  (assert-error
    (fn []
      (parse-with-env "1 +" {}))
    "end of input")

  (suite "pin forms wrap spliced AST")
  (let [env {"MDPS" "DPS * Max Hits"}
        (ast _) (parse-with-env "$MDPS@5$" env)]
    (assert-eq ast
               [:pin 5 nil
                     [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]]
               "$MDPS@5$")
    (let [(ast2 _) (parse-with-env "$MDPS@3@Top Path$" env)]
      (assert-eq ast2
                 [:pin 3 "Top Path"
                       [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]]
                 "$MDPS@3@Top Path$")))

  (suite "FNC-TOTALPRICE becomes intrinsic leaf")
  (let [(ast _) (parse-with-env "$FNC-TOTALPRICE$ / DPS" {})]
    (assert-eq ast
               [:binop "/"
                       [:intrinsic :totalprice]
                       [:ident "DPS"]]
               "intrinsic totalprice"))
  ;; bare $FNC-TOTALPRICE$ (no TP alias) still resolves — Sledger cells
  (assert-eq (parse-var "FNC-TOTALPRICE" {} {} [])
             [:intrinsic :totalprice]
             "top-level FNC-TOTALPRICE")

  (suite "FNC-TOTAL-EXP becomes intrinsic total")
  (let [(ast _) (parse-with-env "$FNC-TOTAL-EXP$" {})]
    (assert-eq ast [:intrinsic :total "EXP"] "TOTAL-EXP"))

  (suite "parse-var-env memoizes all user vars")
  (let [env {"MDPS" "DPS * Max Hits"
             "MCE" "Total Price / $MDPS$"
             "FNC-COST" "100; 200; 300"}
        cache (parse-var-env env)]
    (assert-eq (. cache "MDPS")
               [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]
               "MDPS cached")
    (assert-eq (. cache "MCE")
               [:binop "/"
                       [:ident "Total Price"]
                       [:binop "*" [:ident "DPS"] [:ident "Max Hits"]]]
               "MCE cached with splice")
    (assert-eq (. cache "FNC-COST") nil "config key skipped"))

  (suite "memoization reuses same AST table for repeated refs")
  (let [env {"X" "1 + 2"
             "Y" "$X$ * $X$"}
        cache (parse-var-env env)
        y (. cache "Y")
        ;; both $X$ sides should be the same cached AST object
        left (. y 3)
        right (. y 4)
        x (. cache "X")]
    (assert-eq left x "left $X$ is cached X")
    (assert-true (= left right) "both $X$ refs share one AST object"))

  true)

{:run run}
