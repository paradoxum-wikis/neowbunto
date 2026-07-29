;; eval unit + tower-ish numbers

(local {: suite : assert-eq : assert-near : assert-error : assert-true}
       (require :test_util))
(local {: eval-node : eval-string : sum-series} (require :eval))
(local {: parse-string : parse-with-env : parse-var-env} (require :parser))

(fn run []
  (suite "num / binop / pow / unop pure arithmetic")
  (assert-eq (eval-node {} [:num 42]) 42 "num")
  (assert-eq (eval-node {} [:binop "+" [:num 1] [:num 2]]) 3 "+")
  (assert-eq (eval-node {} [:binop "*"
                                    [:binop "/" [:num 18275] [:num 36]]
                                    [:num 6]])
             (* (/ 18275 36) 6)
             "18275/36*6 left-assoc")
  (assert-eq (eval-node {} [:pow [:num 2] [:pow [:num 3] [:num 2]]])
             512
             "2**(3**2) right-assoc")
  (assert-eq (eval-node {} [:pow [:unop "-" [:num 2]] [:num 2]])
             4
             "(-2)**2")
  (assert-eq (eval-node {} [:binop "+"
                                    [:num 1]
                                    [:binop "*" [:num 2] [:num 3]]])
             7
             "1+2*3")

  (suite "row-lookup")
  (let [ctx {:row {"Damage" 10 "Firerate" 2 "Total Price" 500}}]
    (assert-eq (eval-node ctx [:ident "Damage"]) 10 "Damage")
    (assert-eq (eval-node ctx [:binop "/"
                                       [:ident "Damage"]
                                       [:ident "Firerate"]])
               5
               "Damage / Firerate")
    (assert-eq (eval-node ctx [:ident "Total Price"]) 500 "multi-word")
    (assert-eq (eval-node {:row {"Cost" "1,000"}} [:ident "Cost"])
               1000
               "comma number"))

  (suite "Level falls back to ctx.level")
  (assert-eq (eval-node {:level 4 :row {}} [:ident "Level"]) 4 "ctx.level")
  (assert-eq (eval-node {:level 0 :row {"Level" 9}} [:ident "Level"])
             9
             "row Level wins")

  (suite "N/A and Money scrape to numbers")
  (assert-eq (eval-node {:row {"X" "N/A"}} [:ident "X"]) 0 "N/A -> 0")
  (assert-eq (eval-node {:row {"Total Price" "{{Money|1,250}}"}}
                        [:ident "Total Price"])
             1250
             "Money scrape")

  (suite "bare formula name when column missing (DPS2)")
  (let [env {"DPS2" "Damage * 2"}
        cache (parse-var-env env)
        ctx {:row {"Damage" 5}
             :formula-env env
             :parse-cache cache
             :formula-asts cache
             :formula-name "CE2"}]
    (assert-eq (eval-node ctx [:ident "DPS2"]) 10 "DPS2 formula fallback"))

  (suite "unresolved identifier is a hard error")
  (assert-error
    #(eval-node {:row {} :formula-name "TCE"} [:ident "Thorns DPS"])
    "unresolved identifier")
  (let [(ok err) (pcall eval-node {:row {} :formula-name "TCE"}
                        [:ident "Thorns DPS"])]
    (assert-true (not ok) "failed")
    (assert-true (: (tostring err) :find "Thorns DPS" 1 true) "names ident")
    (assert-true (: (tostring err) :find "TCE" 1 true) "names formula"))

  (suite "dotref via table-cache")
  (let [ctx {:level 0
             :branch ""
             :table-cache {"Base Stats" {0 {"Damage" 10 "Firerate" 1.5}}}}]
    (assert-eq (eval-node ctx [:dotref "Base Stats" "Damage"]) 10 "dotref")
    (assert-eq (eval-node ctx
                          [:binop "/"
                                  [:dotref "Base Stats" "Damage"]
                                  [:dotref "Base Stats" "Firerate"]])
               (/ 10 1.5)
               "Base Stats.Damage / Base Stats.Firerate"))

  (suite "dotref missing is error; branch key form")
  (assert-error
    #(eval-node {:level 0 :table-cache {}}
                [:dotref "Nope" "Col"])
    "unresolved table lookup")
  (let [ctx {:level 2
             :branch "A"
             :table-cache {"Path" {"2A" {"DPS" 99} 2 {"DPS" 1}}}}]
    (assert-eq (eval-node ctx [:dotref "Path" "DPS"]) 99 "level+branch key"))

  (suite "pin overrides level/branch for subtree")
  (let [ctx {:level 0
             :branch ""
             :table-cache {"S" {0 {"V" 1} 5 {"V" 50} "5A" {"V" 77}}}}]
    (assert-eq (eval-node ctx [:pin 5 nil [:dotref "S" "V"]]) 50 "pin level")
    (assert-eq (eval-node ctx [:pin 5 "A" [:dotref "S" "V"]]) 77 "pin branch")
    (assert-eq (eval-node ctx [:dotref "S" "V"]) 1 "outer still L0"))

  (suite "literal returns string; totalprice / total intrinsics")
  (assert-eq (eval-node {} [:literal "<ref>x</ref>"])
             "<ref>x</ref>"
             "literal")
  (assert-eq (eval-node {:costs [375 200 700] :level 0} [:intrinsic :totalprice])
             375
             "TOTALPRICE L0")
  (assert-eq (eval-node {:costs [375 200 700] :level 2} [:intrinsic :totalprice])
             (+ 375 200 700)
             "TOTALPRICE L2")
  (assert-eq (eval-node {:totals {"EXP" [10 20 30]} :level 1}
                        [:intrinsic :total "EXP"])
             30
             "TOTAL-EXP L1")

  (suite "sum-series with schema branches")
  ;; N;N;A;A costs — branch A needs level >= 2 for first A cost
  (assert-eq (sum-series [100 200 50 60] 1 "" ["N" "N" "A" "A"])
             300
             "trunk L1")
  (assert-eq (sum-series [100 200 50 60] 0 "A" ["N" "N" "A" "A"])
             300
             "branch A at L0: trunk only")
  (assert-eq (sum-series [100 200 50 60] 2 "A" ["N" "N" "A" "A"])
             (+ 100 200 50)
             "branch A at L2: trunk + first A")

  (suite "div by zero / unspliced varref errors")
  (assert-error #(eval-node {} [:binop "/" [:num 1] [:num 0]]) "division by zero")
  (assert-error #(eval-node {} [:varref "FNC-COST"]) "unspliced")

  (suite "integration: pure arithmetic string")
  (assert-eq (eval-string "2+3*4" {}) 14 "2+3*4")
  (assert-eq (eval-string "18275/36*6" {}) (* (/ 18275 36) 6) "left-assoc")
  (assert-eq (eval-string "2**3**2" {}) 512 "** right-assoc")
  (assert-eq (eval-string "5 + 5 - 2**5" {}) (- (+ 5 5) (^ 2 5)) "help EQ")

  (suite "integration: Damage / Firerate from row")
  (assert-eq (eval-string "Damage / Firerate"
                          {:row {"Damage" 20 "Firerate" 1.4}})
             (/ 20 1.4)
             "Harvester L0 DPS shape")

  ;; Gladiator L0 MCE = 525 / ((5/0.95)*2) = 49.875
  (suite "integration: Gladiator L0 MCE via $MDPS$ splice")
  (let [env {"DPS" "Damage / Swingrate"
             "MDPS" "$DPS$ * Max Hits"
             "MCE" "Total Price / $MDPS$"}
        ctx {:row {"Damage" 5
                   "Swingrate" 0.95
                   "Max Hits" 2
                   "Total Price" 525}
             :level 0
             :formula-name "MCE"}
        (ast _) (parse-with-env (. env "MCE") env)]
    (assert-near (eval-node ctx ast) 49.875 1e-9
                 "Gladiator L0 MCE = 49.875")
    (let [(a1 _) (parse-with-env "Total Price / $MDPS$" env)
          (a2 _) (parse-with-env "Total Price / ($MDPS$)" env)]
      (assert-near (eval-node ctx a1) (eval-node ctx a2) 1e-12
                   "bare vs paren MCE same value")))

  (suite "integration: Electroshocker L0 CE and MDPS")
  (let [env {"DPS" "Damage / Firerate"
             "MDPS" "DPS * Max Hits"
             "CE" "Total Price / DPS"}
        row {"Damage" 2 "Firerate" 0.75 "Max Hits" 1 "Total Price" 375
             "DPS" (/ 2 0.75)}
        ctx {:row row :level 0}]
    (assert-near (eval-string (. env "CE") ctx) 140.625 1e-9 "CE")
    (assert-near (eval-string (. env "MDPS") ctx) (* (/ 2 0.75) 1) 1e-9
                 "MDPS")
    (let [env2 {"DPS" "Damage / Firerate"
                "MDPS" "$DPS$ * Max Hits"}]
      (assert-near (eval-string "$MDPS$" ctx env2)
                   (* (/ 2 0.75) 1) 1e-9
                   "MDPS via splice")))

  ;; Harvester L0 TCE = 2000 / (2/0.25) = 250
  (suite "integration: Harvester L0 TCE = 250")
  (let [env {"TDPS" "Thorns Damage / Thorns Tick"
             "TP" "$FNC-TOTALPRICE$"
             "TCE" "$TP$ / $TDPS$"}
        ctx {:row {"Thorns Damage" 2 "Thorns Tick" 0.25}
             :level 0
             :costs [2000 625 1500 4000 8750 24300]
             :formula-name "TCE"}
        (ast _) (parse-with-env (. env "TCE") env)]
    (assert-eq (eval-node ctx ast) 250 "Harvester L0 TCE")
    (assert-eq (eval-string "$TCE$" ctx env) 250 "via $TCE$ varref"))

  ;; left-assoc string expand would give 400 not 25
  (suite "MCE grouping fix: not left-assoc string expand")
  (let [env {"MDPS" "DPS * Max Hits"
             "MCE" "Total Price / $MDPS$"}
        ctx {:row {"DPS" 10 "Max Hits" 4 "Total Price" 1000}}
        right (eval-string (. env "MCE") ctx env)
        buggy (eval-string "Total Price / DPS * Max Hits" ctx)]
    (assert-eq right (/ 1000 (* 10 4)) "correct = 25")
    (assert-eq buggy (* (/ 1000 10) 4) "buggy = 400")
    (assert-true (not= right buggy) "fix changes the number"))

  (suite "parse-var-env + eval whole tower formula set")
  (let [env {"DPS" "Damage / Swingrate"
             "MDPS" "$DPS$ * Max Hits"
             "MCE" "Total Price / $MDPS$"
             "TP" "$FNC-TOTALPRICE$"}
        cache (parse-var-env env)
        ctx {:row {"Damage" 5 "Swingrate" 0.95 "Max Hits" 2 "Total Price" 525}
             :level 0
             :costs [525 450 1250]}]
    (assert-near (eval-node ctx (. cache "DPS")) (/ 5 0.95) 1e-12 "DPS")
    (assert-near (eval-node ctx (. cache "MDPS")) (* (/ 5 0.95) 2) 1e-12 "MDPS")
    (assert-near (eval-node ctx (. cache "MCE")) 49.875 1e-9 "MCE")
    (assert-eq (eval-node ctx (. cache "TP")) 525 "TP L0"))

  true)

{:run run}
