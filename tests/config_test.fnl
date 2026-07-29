;; FNC/FSE config + TOTAL* wiring

(local {: suite : assert-eq : assert-near : assert-true : assert-error}
       (require :test_util))
(local {: parse-vars : build-config : parse-page-config : split-semi
        : is-numeric-array-body : parse-number-list : get-var-key
        : get-array-var-key : formula-env : config-name?}
       (require :config))
(local {: eval-node : make-ctx : sum-total} (require :eval))
(local {: parse-with-env : parse-var-env} (require :parser))

(local electroshocker-var
  "<var>
$DPS$ = Damage / Firerate
$MDPS$ = DPS * Max Hits
$CE$ = Total Price / DPS
$TP$ = $FNC-TOTALPRICE$
$FNC-COST$ = 375; 200; 700; 2500; 6350; 16935
$FNC-DETECTION$ = 2; 0;
$FNC-UPGRADE$ = Improved Handling; Higher Voltage; Faraday's Vest; Shock Force; Zeus Cannon
$FNC-ROFBUG$ = Firerate
$DREF$ = <ref>Assumes that only one enemy is hit per shock.</ref>
$MREF$ = <ref>Calculated as DPS × Max Hits.</ref>
</var>")

(local branched-var
  "<var>
$FNC-SCHEMA$ = N; N; A; A; B; B
$FNC-BRANCH$ = Top Path Stats; Bottom Path Stats
$FNC-COST$ = 100; 200; 50; 60; 70; 80
$FNC-INDEX$ = Sentry Stats.Level
$FSE-CATEGORY$ = Golden Perks
$FSE-DETECTION$ = 3; ; 0
</var>")

(fn run []
  (suite "split-semi preserves empty slots")
  (assert-eq (split-semi "3; ; 0") ["3" "" "0"] "detection-style")
  (assert-eq (split-semi "a;b;c") ["a" "b" "c"] "plain")

  (suite "config-name? vs formula-name?")
  (assert-true (config-name? "FNC-COST") "FNC-COST")
  (assert-true (config-name? "FSE-CATEGORY") "FSE")
  (assert-true (config-name? "ROFBUG-2020") "ROFBUG")
  (assert-true (not (config-name? "MDPS")) "MDPS is formula")
  (assert-true (not (config-name? "TP")) "TP is formula")

  (suite "is-numeric-array-body")
  (assert-true (is-numeric-array-body "375; 200; 700") "costs")
  (assert-true (is-numeric-array-body "1000") "single")
  (assert-true (not (is-numeric-array-body "")) "empty is not a series")
  (assert-true (not (is-numeric-array-body "Damage / Firerate")) "formula")
  (assert-true (not (is-numeric-array-body "{{#expr:1+2}}")) "expr")
  (assert-true (not (is-numeric-array-body "100; foo; 200")) "garbage segment")

  (suite "parse-number-list blanks and hard errors")
  (assert-eq (parse-number-list "100;;200") [100 0 200] "blank slots -> 0")
  (assert-eq (parse-number-list "1,000; 2") [1000 2] "commas")
  (assert-error (fn [] (parse-number-list "100; foo; 200")) "foo")

  (suite "parse-vars Electroshocker")
  (let [(vars is-fse) (parse-vars electroshocker-var)]
    (assert-eq is-fse false "no FSE")
    (assert-eq (. vars "$FNC-COST$") "375; 200; 700; 2500; 6350; 16935" "cost raw")
    (assert-eq (. vars "$DPS$") "Damage / Firerate" "DPS formula")
    (assert-eq (. vars "$TP$") "$FNC-TOTALPRICE$" "TP alias")
    (assert-true (not= (. vars "$DREF$") nil) "ref present")
    (let [env (formula-env vars)]
      (assert-eq (. env "DPS") "Damage / Firerate" "formula-env DPS")
      (assert-eq (. env "FNC-COST") nil "COST excluded from formula-env")
      (assert-eq (. env "DREF") (. vars "$DREF$") "literal ref kept as formula-env entry")))

  (suite "build-config costs / rof / no schema")
  (let [(vars _) (parse-vars electroshocker-var)
        cfg (build-config vars "")]
    (assert-eq cfg.costs [375 200 700 2500 6350 16935] "costs list")
    (assert-eq cfg.schema nil "no schema")
    (assert-eq (. cfg.rof-cols "Firerate") true "rof col")
    (assert-eq cfg.rof-offset nil "modern rof no fixed offset"))

  (suite "build-config schema branch map index fse")
  (let [(vars is-fse) (parse-vars branched-var)
        cfg (build-config vars "")]
    (assert-true is-fse "has FSE")
    (assert-eq cfg.schema ["N" "N" "A" "A" "B" "B"] "schema")
    (assert-eq (. cfg.branch-map "Top Path Stats") "A" "branch A")
    (assert-eq (. cfg.branch-map "Bottom Path Stats") "B" "branch B")
    (assert-eq (. cfg.index 1)
               {:table "Sentry Stats" :col "Level"}
               "INDEX")
    (assert-eq (. cfg.fse "CATEGORY") "Golden Perks" "FSE category")
    (assert-eq (. cfg.fse "DETECTION") "3; ; 0" "FSE detection raw")
    (assert-eq cfg.costs [100 200 50 60 70 80] "branched costs"))

  (suite "PVP prefix COST fallback")
  (let [vars {"$FNC-COST$" "1; 2; 3"
              "$FNC-PVP-COST$" "10; 20; 30"}]
    (assert-eq (get-var-key vars "PVP" "COST") "$FNC-PVP-COST$" "prefers PVP")
    (assert-eq (get-var-key vars "" "COST") "$FNC-COST$" "default")
    (assert-eq (get-var-key {"$FNC-COST$" "1"} "PVP" "COST")
               "$FNC-COST$"
               "fallback to unprefixed"))

  (suite "get-array-var-key for TOTAL-X")
  (let [vars {"$EXP$" "10; 20"
              "$PVP-EXP$" "1; 2"}]
    (assert-eq (get-array-var-key vars "" "EXP") "$EXP$" "plain")
    (assert-eq (get-array-var-key vars "PVP" "EXP") "$PVP-EXP$" "prefixed"))

  (suite "TOTALPRICE via make-ctx + config")
  (let [(cfg _) (parse-page-config electroshocker-var "")
        ctx0 (make-ctx {:config cfg :level 0})
        ctx2 (make-ctx {:config cfg :level 2})]
    (assert-eq (eval-node ctx0 [:intrinsic :totalprice]) 375 "L0")
    (assert-eq (eval-node ctx2 [:intrinsic :totalprice])
               (+ 375 200 700)
               "L2"))

  (suite "TOTALPRICE with schema branch")
  (let [(cfg _) (parse-page-config branched-var "")
        trunk (make-ctx {:config cfg :level 1 :branch ""})
        br-a (make-ctx {:config cfg :level 2 :branch "A"})]
    (assert-eq (eval-node trunk [:intrinsic :totalprice]) 300 "trunk L1")
    (assert-eq (eval-node br-a [:intrinsic :totalprice])
               (+ 100 200 50)
               "branch A L2"))

  (suite "TOTAL-X numeric array series")
  (let [vars {"$EXP$" "10; 20; 30; 40"}
        cfg (build-config vars "")
        ctx (make-ctx {:config cfg :level 2 :vars vars})]
    (assert-eq (sum-total ctx "EXP") 60 "10+20+30"))

  (suite "TOTAL-X missing series is a hard error")
  (let [vars {}
        cfg (build-config vars "")
        ctx (make-ctx {:config cfg :level 1 :vars vars})]
    (assert-error (fn [] (sum-total ctx "EXP")) "no numeric series"))

  (suite "TOTAL-X formula series sums per-level AST")
  ;; $EXP$ = 10 * (Level + 1)  -> L0=10, L1=20, L2=30; TOTAL-EXP@2 = 60
  (let [vars {"$EXP$" "10 * (Level + 1)"}
        env (formula-env vars)
        cache (parse-var-env env)
        cfg (build-config vars "")
        ctx (make-ctx {:config cfg
                       :level 2
                       :vars vars
                       :formula-asts cache
                       :parse-cache cache
                       :row {}})]
    (assert-eq (sum-total ctx "EXP") 60 "formula series")
    (assert-eq (eval-node ctx [:intrinsic :total "EXP"]) 60 "via AST"))

  (suite "pin TOTALPRICE@N via branch display name")
  (let [(cfg _) (parse-page-config branched-var "")
        ctx (make-ctx {:config cfg :level 0 :branch ""})
        node [:pin 2 "Top Path Stats" [:intrinsic :totalprice]]]
    (assert-eq (eval-node ctx node)
               (+ 100 200 50)
               "pin resolves display name -> A"))

  (suite "Electroshocker TP end-to-end through formula env")
  (let [(cfg _) (parse-page-config electroshocker-var "")
        env cfg.formula-env
        (ast _) (parse-with-env "$TP$" env)
        ctx (make-ctx {:config cfg :level 0 :row {}})]
    (assert-eq (eval-node ctx ast) 375 "TP -> TOTALPRICE L0")
    (let [ctx5 (make-ctx {:config cfg :level 5 :row {}})]
      (assert-eq (eval-node ctx5 ast)
                 (+ 375 200 700 2500 6350 16935)
                 "TP L5 full sum")))

  (suite "config keys never go through expression parser")
  ;; COST is a list not a formula even with semicolons
  (let [(cfg _) (parse-page-config
                  "<var>\n$FNC-COST$ = 1; 2; 3\n$FNC-SCHEMA$ = N; A; A\n</var>"
                  "")]
    (assert-eq cfg.costs [1 2 3] "parsed as list")
    (assert-eq cfg.schema ["N" "A" "A"] "schema list")
    (assert-eq (. cfg.formula-env "FNC-COST") nil "not in formula-env"))

  (suite "multiline quoted var")
  (let [(vars _) (parse-vars "<var>
$Note$ = \"line one
line two\"
$X$ = 1
</var>")]
    (assert-true (: (. vars "$Note$") :find "line one" 1 true) "line one")
    (assert-true (: (. vars "$Note$") :find "line two" 1 true) "line two")
    (assert-eq (. vars "$X$") "1" "next var ok"))

  true)

{:run run}
