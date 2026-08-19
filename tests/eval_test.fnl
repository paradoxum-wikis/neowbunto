;; eval unit + tower-ish numbers

(local {: suite : assert-eq : assert-near : assert-error : assert-true}
       (require :test_util))

(local {: eval-node : eval-string : sum-series : expand-inline-expr}
       (require :eval))

(local {: parse-with-env : parse-var-env} (require :parser))

(fn run []
  (suite "num / binop / pow / unop pure arithmetic")
  (assert-eq (eval-node {} [:num 42]) 42 "num")
  (assert-eq (eval-node {} [:binop "+" [:num 1] [:num 2]]) 3 "+")
  (assert-eq (eval-node {} [:binop
                            "*"
                            [:binop "/" [:num 18275] [:num 36]]
                            [:num 6]]) (* (/ 18275 36) 6)
             "18275/36*6 left-assoc")
  (assert-eq (eval-node {} [:pow [:num 2] [:pow [:num 3] [:num 2]]]) 512
             "2**(3**2) right-assoc")
  (assert-eq (eval-node {} [:pow [:unop "-" [:num 2]] [:num 2]]) 4 "(-2)**2")
  (assert-eq (eval-node {} [:binop "+" [:num 1] [:binop "*" [:num 2] [:num 3]]])
             7 "1+2*3")
  (suite "row-lookup")
  (let [ctx {:row {"Damage" 10 "Firerate" 2 "Total Price" 500}}]
    (assert-eq (eval-node ctx [:ident "Damage"]) 10 "Damage")
    (assert-eq (eval-node ctx
                          [:binop "/" [:ident "Damage"] [:ident "Firerate"]])
               5 "Damage / Firerate")
    (assert-eq (eval-node ctx [:ident "Total Price"]) 500 "multi-word")
    (assert-eq (eval-node {:row {"Cost" "1,000"}} [:ident "Cost"]) 1000
               "comma number"))
  (suite "Level falls back to ctx.level")
  (assert-eq (eval-node {:level 4 :row {}} [:ident "Level"]) 4 "ctx.level")
  (assert-eq (eval-node {:level 0 :row {"Level" 9}} [:ident "Level"]) 9
             "row Level wins")
  (suite "N/A and Money scrape to numbers")
  (assert-eq (eval-node {:row {"X" "N/A"}} [:ident "X"]) 0 "N/A -> 0")
  (assert-eq (eval-node {:row {"Total Price" "{{Money|1,250}}"}}
                        [:ident "Total Price"]) 1250
             "Money scrape")
  (suite "bare VAR is a column, not $VAR$")
  (let [env {"VAR" "1" "DPS" "1 + VAR"}
        cache (parse-var-env env)
        ctx {:row {}
             :formula-env env
             :parse-cache cache
             :formula-asts cache
             :formula-name "DPS"}]
    (assert-error #(eval-node ctx (. cache "DPS")) "unresolved identifier")
    (assert-error #(eval-node ctx [:ident "VAR"]) "unresolved identifier"))
  (suite "$VAR$ paste still works; column VAR still works")
  (let [env {"VAR" "1" "DPS" "1 + $VAR$"}
        cache (parse-var-env env)
        ctx {:row {} :formula-env env :parse-cache cache :formula-asts cache}]
    (assert-eq (eval-node ctx (. cache "DPS")) 2 "1 + $VAR$")
    (assert-eq (eval-node {:row {"VAR" 10}}
                          [:binop "+" [:num 1] [:ident "VAR"]])
               11 "column VAR"))
  (suite "cell still holding $NAME$ is eval'd (Table.Col / cached row)")
  ;; $DPS2$ would scrape the trailing 2 in extract-number
  (let [env {"SDPS" "Damage * 2"}
        cache (parse-var-env env)
        ctx {:row {"Damage" 5 "X" "$SDPS$"}
             :formula-env env
             :parse-cache cache
             :formula-asts cache}]
    (assert-eq (eval-node ctx [:ident "X"]) 10 "cell $SDPS$"))
  (suite "unresolved identifier is a hard error")
  (assert-error #(eval-node {:row {} :formula-name "TCE"} [:ident "Thorns DPS"])
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
                          [:binop
                           "/"
                           [:dotref "Base Stats" "Damage"]
                           [:dotref "Base Stats" "Firerate"]])
               (/ 10 1.5) "Base Stats.Damage / Base Stats.Firerate"))
  (suite "dotref missing is error; branch key form")
  (assert-error #(eval-node {:level 0 :table-cache {}} [:dotref "Nope" "Col"])
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
  (assert-eq (eval-node {} [:literal "<ref>x</ref>"]) "<ref>x</ref>" "literal")
  (assert-eq (eval-node {:costs [375 200 700] :level 0}
                        [:intrinsic :totalprice]) 375
             "TOTALPRICE L0")
  (assert-eq (eval-node {:costs [375 200 700] :level 2}
                        [:intrinsic :totalprice]) (+ 375 200 700)
             "TOTALPRICE L2")
  (assert-eq (eval-node {:totals {"EXP" [10 20 30]} :level 1}
                        [:intrinsic :total "EXP"]) 30
             "TOTAL-EXP L1")
  (suite "sum-series with schema branches")
  ;; N;N;A;A costs
  ;; branch A needs level >= 2 for first A cost
  (assert-eq (sum-series [100 200 50 60] 1 "" ["N" "N" "A" "A"]) 300 "trunk L1")
  (assert-eq (sum-series [100 200 50 60] 0 "A" ["N" "N" "A" "A"]) 300
             "branch A at L0: trunk only")
  (assert-eq (sum-series [100 200 50 60] 2 "A" ["N" "N" "A" "A"])
             (+ 100 200 50) "branch A at L2: trunk + first A")
  (suite "div by zero / unspliced varref errors")
  (assert-error #(eval-node {} [:binop "/" [:num 1] [:num 0]])
                "division by zero")
  (assert-error #(eval-node {} [:varref "FNC-COST"]) "unspliced")
  (suite "integration: pure arithmetic string")
  (assert-eq (eval-string "2+3*4" {}) 14 "2+3*4")
  (assert-eq (eval-string "18275/36*6" {}) (* (/ 18275 36) 6) "left-assoc")
  (assert-eq (eval-string "2**3**2" {}) 512 "** right-assoc")
  (assert-eq (eval-string "5 + 5 - 2**5" {}) (- (+ 5 5) (^ 2 5)) "help EQ")
  (suite "integration: Damage / Firerate from row")
  (assert-eq (eval-string "Damage / Firerate"
                          {:row {"Damage" 20 "Firerate" 1.4}})
             (/ 20 1.4) "Harvester L0 DPS shape")
  ;; $MCE$ = Total Price / Max DPS (column filled by $MDPS$ cell)
  (suite "integration: Gladiator L0 MCE via Max DPS column")
  (let [env {"DPS" "Damage / Swingrate"
             "MDPS" "$DPS$ * Max Hits"
             "MCE" "Total Price / Max DPS"
             "CE" "Total Price / DPS"}
        mdps (* (/ 5 0.95) 2)
        ctx {:row {"Damage" 5
                   "Swingrate" 0.95
                   "Max Hits" 2
                   "Total Price" 525
                   "Max DPS" mdps
                   "DPS" (/ 5 0.95)}
             :level 0
             :formula-name "MCE"}
        (ast _) (parse-with-env (. env "MCE") env)]
    (assert-near (eval-node ctx ast) 49.875 1e-9 "Gladiator L0 MCE")
    (assert-near (eval-string (. env "CE") ctx) (/ 525 (/ 5 0.95)) 1e-9 "CE")
    (assert-near (eval-string (. env "MDPS") ctx env) mdps 1e-9 "MDPS cell"))
  ;; MDPS = DPS * Max Hits, MCE = Total Price / Max DPS
  (suite "integration: Electroshocker L0 CE and MDPS")
  (let [env {"DPS" "Damage / Firerate"
             "MDPS" "DPS * Max Hits"
             "CE" "Total Price / DPS"
             "MCE" "Total Price / Max DPS"}
        dps (/ 5 1.35)
        row {"Damage" 5
             "Firerate" 1.35
             "Max Hits" 2
             "Total Price" 650
             "DPS" dps
             "Max DPS" (* dps 2)}
        ctx {:row row :level 0}]
    (assert-near (eval-string (. env "CE") ctx) (/ 650 dps) 1e-9 "CE")
    (assert-near (eval-string (. env "MDPS") ctx) (* dps 2) 1e-9 "MDPS")
    (assert-near (eval-string (. env "MCE") ctx) (/ 650 (* dps 2)) 1e-9 "MCE"))
  ;; $TCE$ = $TP$ / Thorns DPS (column and not $TDPS$ paste)
  (suite "integration: Harvester L0 TCE = 250")
  (let [env {"TDPS" "Thorns Damage / Thorns Tick"
             "TP" "$FNC-TOTALPRICE$"
             "TCE" "$TP$ / Thorns DPS"}
        ctx {:row {"Thorns Damage" 2
                   "Thorns Tick" 0.25
                   "Thorns DPS" (/ 2 0.25)}
             :level 0
             :costs [2000 625 1500 4000 8750 24300]
             :formula-name "TCE"}
        (ast _) (parse-with-env (. env "TCE") env)]
    (assert-eq (eval-node ctx ast) 250 "Harvester L0 TCE")
    (assert-eq (eval-string "$TCE$" ctx env) 250 "via $TCE$"))
  ;; bare paste is left-assoc (i.e. Combatant.wiki still has / $MDPS$)
  (suite "bare $MDPS$ text-macro is left-assoc (same as string expand)")
  (let [env {"MDPS" "DPS * Max Hits" "MCE" "Total Price / $MDPS$"}
        ctx {:row {"DPS" 10 "Max Hits" 4 "Total Price" 1000}}
        bare (eval-string (. env "MCE") ctx env)
        explicit (eval-string "Total Price / DPS * Max Hits" ctx)
        grouped (eval-string "Total Price / ($MDPS$)" ctx env)]
    (assert-eq bare (* (/ 1000 10) 4) "bare = 400")
    (assert-eq bare explicit "matches string left-assoc")
    (assert-eq grouped (/ 1000 (* 10 4)) "parens = 25"))
  (suite "parse-var-env + eval whole tower formula set")
  (let [env {"DPS" "Damage / Swingrate"
             "MDPS" "$DPS$ * Max Hits"
             "MCE" "Total Price / Max DPS"
             "TP" "$FNC-TOTALPRICE$"}
        cache (parse-var-env env)
        ctx {:row {"Damage" 5
                   "Swingrate" 0.95
                   "Max Hits" 2
                   "Total Price" 525
                   "Max DPS" (* (/ 5 0.95) 2)}
             :level 0
             :costs [525 450 1250]}]
    (assert-near (eval-node ctx (. cache "DPS")) (/ 5 0.95) 1e-12 "DPS")
    (assert-near (eval-node ctx (. cache "MDPS")) (* (/ 5 0.95) 2) 1e-12 "MDPS")
    (assert-near (eval-node ctx (. cache "MCE")) 49.875 1e-9 "MCE")
    (assert-eq (eval-node ctx (. cache "TP")) 525 "TP L0"))
  ;; row already has Damage=n while #expr still says Table.Critical Damage
  (suite "mw-expr: local Damage does not eat Table.Critical Damage")
  (let [env {"PCDMG" "{{#expr:floor(Warden Stats.Damage * (2 - 1) + Warden Stats.Critical Damage)}}"
             "CDMG" "{{#expr:Damage * Critical Hit Multiplier round 0}}"}
        cache (parse-var-env env)
        tc {"Warden Stats" {4 {"Damage" 80
                               "Critical Damage" "$CDMG$"
                               "Critical Hit Multiplier" 3}}}
        ctx {:row {"Level" 4 "Damage" 160}
             :level 4
             :table-cache tc
             :formula-env env
             :parse-cache cache
             :formula-asts cache}]
    (assert-eq (eval-node ctx (. cache "PCDMG")) 320
               "PCDMG with local Damage=160 still uses remote Critical Damage"))
  (suite "dotref scrapes percent suffix")
  (let [ctx {:level 0
             :branch ""
             :table-cache {"Operator Stats" {0 {"Coordination Damage Boost" "10%"}}}}]
    (assert-eq (eval-node ctx
                          [:dotref
                           "Operator Stats"
                           "Coordination Damage Boost"]) 10
               "10%")
    (assert-eq (eval-node ctx [:binop
                               "*"
                               [:dotref
                                "Operator Stats"
                                "Coordination Damage Boost"]
                               [:num 4]]) 40 "10% * 4"))
  (suite "expand-inline-expr materializes Table.Col")
  (let [ctx {:level 2
             :row {}
             :table-cache {"Operator Stats" {2 {"Damage" 4
                                                "Coordination Damage Boost" "10%"}}}}]
    (assert-eq (expand-inline-expr "{{#expr: Operator Stats.Damage * 2}}" ctx)
               "8" "2*4")
    (assert-eq (expand-inline-expr "{{#expr:floor(Operator Stats.Damage * (1 + Operator Stats.Coordination Damage Boost * 0.01))}}"
                                   ctx) "4" "floor 4.4"))
  (suite "wikitext leftover #expr materializes Table.Col")
  (let [env {"BD" "{{Exp|{{#expr:floor(Operator Stats.Damage * (1 + Operator Stats.Coordination Damage Boost * 0.01))}}}}"}
        cache (parse-var-env env)
        ctx {:level 2
             :row {}
             :table-cache {"Operator Stats" {2 {"Damage" 4
                                                "Coordination Damage Boost" "10%"}}}
             :formula-env env
             :parse-cache cache
             :formula-asts cache}]
    (assert-eq (. cache "BD" 1) :wikitext "wikitext node")
    (assert-eq (eval-node ctx (. cache "BD")) "{{Exp|4}}" "floor 4*1.1"))
  (suite "#expr evals $VAR$ to a number and not paste")
  (let [env {"DMG" "Damage" "BD" "{{#expr: floor($DMG$ * (1 + 0.4))}}"}
        cache (parse-var-env env)
        ctx {:level 0
             :row {"Damage" 6}
             :formula-env env
             :parse-cache cache
             :formula-asts cache}]
    (assert-eq (eval-node ctx (. cache "BD")) 8 "floor(6*1.4)"))
  (suite "#expr $COST@N$ pin")
  (let [env {"COST" "100; 200; 300" "X" "{{#expr: $COST@1$ * 2}}"}
        cache (parse-var-env env)
        ctx {:level 0
             :branch ""
             :formula-env env
             :parse-cache cache
             :formula-asts cache}]
    (assert-eq (eval-node ctx (. cache "X")) 400 "pin 1 * 2"))
  (suite "#expr missing $VAR$ is a hard error")
  (let [env {"BD" "{{#expr: $NOPE$ * 2}}"}
        cache (parse-var-env env)
        ctx {:level 0
             :row {}
             :formula-env env
             :parse-cache cache
             :formula-asts cache}]
    (assert-error #(eval-node ctx (. cache "BD")) "undefined variable"))
  (suite "array var: numeric ; list is the element at level")
  (let [env {"COST" "100; 200; 300"}
        cache (parse-var-env env)
        ctx {:level 0
             :branch ""
             :formula-env env
             :parse-cache cache
             :formula-asts cache}]
    (assert-eq (. cache "COST") [:array "100; 200; 300"] "COST parses as array")
    (assert-eq (eval-node ctx (. cache "COST")) 100 "L0")
    (set ctx.level 1)
    (assert-eq (eval-node ctx (. cache "COST")) 200 "L1")
    (set ctx.level 2)
    (assert-eq (eval-node ctx (. cache "COST")) 300 "L2"))
  (suite "array var: $X@N$ pins; out of range is hard error")
  (let [env {"COST" "100; 200; 300"}
        ctx {:level 0 :branch ""}]
    (assert-eq (eval-string "$COST@1$" ctx env) 200 "pin 1")
    (assert-eq (eval-string "$COST@2$" ctx env) 300 "pin 2")
    (assert-error #(eval-string "$COST@5$" ctx env) "out of range"))
  (suite "array var: mixed list is whole text unless pinned")
  (let [env {"NOTE" "120; n/a; hey; 400"}
        cache (parse-var-env env)
        ctx {:level 1
             :branch ""
             :formula-env env
             :parse-cache cache
             :formula-asts cache}]
    (assert-eq (eval-node ctx (. cache "NOTE")) "120; n/a; hey; 400"
               "bare mixed = whole text")
    (assert-eq (eval-string "$NOTE@1$" ctx env) "n/a" "pin mixed string")
    (assert-eq (eval-string "$NOTE@3$" ctx env) 400 "pin mixed number"))
  (suite "array var: $COST$ inside a formula")
  (let [env {"COST" "100; 200; 300" "DOUBLE" "$COST$ * 2"}
        ctx {:level 1 :branch ""}]
    (assert-eq (eval-string "$DOUBLE$" ctx env) 400 "$COST$ * 2 at L1"))
  (suite "FNC-TOTAL-COST sums $COST$; FNC-TOTALPRICE still only FNC-COST")
  (let [vars {"$COST$" "100; 200; 300"}
        env {"COST" "100; 200; 300"}
        cache (parse-var-env env)
        ctx {:level 2
             :branch ""
             :vars vars
             :formula-env env
             :parse-cache cache
             :formula-asts cache}]
    (assert-eq (eval-node ctx [:intrinsic :total "COST"]) 600 "TOTAL-COST")
    (assert-error #(eval-node ctx [:intrinsic :totalprice]) "needs ctx.costs"))
  (suite "array var: schema N;N;A;B;B")
  (let [env {"COST" "10; 20; 30; 40; 50"}
        cache (parse-var-env env)
        schema ["N" "N" "A" "B" "B"]
        ctx {:level 0
             :branch ""
             :schema schema
             :formula-env env
             :parse-cache cache
             :formula-asts cache}]
    (assert-eq (eval-node ctx (. cache "COST")) 10 "trunk L0")
    (set ctx.level 1)
    (assert-eq (eval-node ctx (. cache "COST")) 20 "trunk L1")
    (set ctx.level 2)
    (set ctx.branch "A")
    (assert-eq (eval-node ctx (. cache "COST")) 30 "A L2")
    (set ctx.branch "B")
    (assert-eq (eval-node ctx (. cache "COST")) 40 "B L2")
    (set ctx.level 3)
    (assert-eq (eval-node ctx (. cache "COST")) 50 "B L3")
    (assert-eq (eval-string "$COST@2@B$" {:level 0 :branch "" :schema schema}
                            env) 40 "$COST@2@B$"))
  true)

{:run run}
