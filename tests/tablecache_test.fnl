;; table cache build + cache-get

(local {: suite : assert-eq : assert-near : assert-true} (require :test_util))
(local {: parse-table : parse-all-tables} (require :wikitable))
(local {: build-table-cache : build-page-cache : build-page-cache-from-text
        : cache-get : rof-bug}
       (require :tablecache))
(local {: eval-node : make-ctx} (require :eval))
(local {: parse-with-env} (require :parser))
(local {: parse-page-config} (require :config))

(local electroshocker-table
  "{| class=\"wikitable stats-table\"
! Level !! Total Price !! Damage !! Firerate !! Range !! Chain Range !! Max Hits !! Shock Time !! Defense Drop !! DPS !! Max DPS !! Cost Efficiency
|-
| 0 || 375 || 2 || 0.75 || 11 || 7 || 1 || 0.05 || 5% || $DPS$ || $MDPS$ || $CE$
|-
| 1 || 575 || 3 || 0.75 || 12.5 || 7 || 1 || 0.05 || 5% || $DPS$ || $MDPS$ || $CE$
|-
| 2 || 1275 || 6 || 0.65 || 12.5 || 7 || 1 || 0.08 || 10% || $DPS$ || $MDPS$ || $CE$
|}")

(local harvester-page
  "<var>
$DPS$ = Damage / Firerate
$TDPS$ = Thorns Damage / Thorns Tick
$TP$ = $FNC-TOTALPRICE$
$FNC-COST$ = 2000; 625; 1500
$FNC-ROFBUG$ = Firerate
</var>
{| class=\"wikitable stats-table\"
! colspan=\"4\" |Harvester Stats
|-
! Level !! Damage !! Firerate !! DPS
|-
| 0 || 20 || 1.4 || $DPS$
|-
| 1 || 20 || 1.2 || $DPS$
|}
{| class=\"wikitable stats-table\"
! colspan=\"5\" |Thorns Stats
|-
! Level !! Thorns Damage !! Thorns Tick !! Thorns DPS || Extra
|-
| 0 || 2 || 0.25 || $TDPS$ || x
|-
| 1 || 4 || 0.25 || $TDPS$ || y
|}")

(fn run []
  (suite "rof-bug modern and offset modes")
  (assert-eq (rof-bug 0 nil) 0 "zero")
  (assert-eq (rof-bug -1 nil) -1 "neg")
  (assert-eq (rof-bug 1 0.05) 1.05 "2019 offset")
  (assert-eq (rof-bug 1 0.03) 1.03 "2020 offset")
  (assert-true (> (rof-bug 0.75 nil) 0.75) "modern rounds firerate up-ish")

  (suite "build-table-cache numeric cells (default resolver)")
  (let [ast (parse-table electroshocker-table {})
        cache (build-table-cache ast "Level" {})]
    (assert-eq (. (cache-get cache 0 "") "Damage") 2 "L0 damage")
    (assert-eq (. (cache-get cache 0 "") "Firerate") 0.75 "L0 firerate")
    (assert-eq (. (cache-get cache 0 "") "Level") 0 "Level field")
    (assert-eq (. (cache-get cache 1 "") "Damage") 3 "L1 damage")
    (assert-eq (. (cache-get cache 2 "") "Total Price") 1275 "L2 TP number")
    (assert-eq (. (cache-get cache 0 "") "DPS") "$DPS$" "unresolved formula kept")
    (assert-eq (. (cache-get cache 0 "") "TotalPrice") 375 "stripped key"))

  (suite "cache-get branch key form")
  (let [ast (parse-table
              "{|\n! Level !! DPS\n|-\n| 2A || 99\n|-\n| 2 || 1\n|}"
              {})
        cache (build-table-cache ast "Level" {})]
    (assert-eq (. cache "2A" "DPS") 99 "branch key")
    (assert-eq (. (cache-get cache 2 "A") "DPS") 99 "cache-get level+branch")
    (assert-eq (. (cache-get cache 2 "") "DPS") 1 "trunk level 2"))

  (suite "range index expands to multiple keys")
  (let [ast (parse-table
              "{|\n! Level !! Damage\n|-\n| 1-3 || 9\n|}"
              {})
        cache (build-table-cache ast "Level" {})]
    (assert-eq (. cache 1 "Damage") 9 "key 1")
    (assert-eq (. cache 2 "Damage") 9 "key 2")
    (assert-eq (. cache 3 "Damage") 9 "key 3"))

  (suite "ROF cols store Header_ROF")
  (let [ast (parse-table electroshocker-table {})
        cache (build-table-cache ast "Level"
                                 {:rof-cols {"Firerate" true}
                                  :rof-offset 0.05})]
    (assert-eq (. (cache-get cache 0 "") "Firerate") 0.75 "norm firerate")
    (assert-near (. (cache-get cache 0 "") "Firerate_ROF") 0.80 1e-9
                 "rof = +0.05"))

  (suite "custom resolve-cell evaluates simple formulas from row")
  (let [ast (parse-table
              "{|\n! Level !! Damage !! Firerate !! DPS\n|-\n| 0 || 20 || 1.4 || $DPS$\n|}"
              {})
        env {"DPS" "Damage / Firerate"}
        resolve (fn [raw _header row _rof?]
                  (if (raw:match "%$[^%$]+%$")
                      (let [name (raw:match "%$([^%$]+)%$")
                            (ast _) (parse-with-env (. env name) env)]
                        (eval-node {:row row :level 0} ast))
                      raw))
        cache (build-table-cache ast "Level" {:resolve-cell resolve})]
    (assert-near (. (cache-get cache 0 "") "DPS") (/ 20 1.4) 1e-9
                 "resolved DPS from row"))

  (suite "build-page-cache multi-table + titles")
  (let [tables (parse-all-tables harvester-page {})
        page (build-page-cache tables {:rof-cols {"Firerate" true}
                                       :rof-offset nil})]
    (assert-true (not= (. page "Harvester Stats") nil) "harvester named")
    (assert-true (not= (. page "Thorns Stats") nil) "thorns named")
    (assert-true (not= (. page "HarvesterStats") nil) "stripped name")
    (assert-eq (. (cache-get (. page "Harvester Stats") 0 "") "Damage") 20
               "harvester L0")
    (assert-eq (. (cache-get (. page "Thorns Stats") 1 "") "Thorns Damage") 4
               "thorns L1")
    (assert-eq (. (cache-get (. page "Thorns Stats") 0 "") "Extra") "x"
               "|| header fix"))

  (suite "build-page-cache-from-text end-to-end")
  (let [page (build-page-cache-from-text harvester-page {} {})]
    (assert-eq (. (cache-get (. page "Harvester Stats") 1 "") "Firerate") 1.2
               "from text"))

  (suite "eval dotref against built page cache")
  (let [page (build-page-cache-from-text
               "{| \n! colspan=\"2\" |Base Stats\n|-\n! Level !! Damage\n|-\n| 0 || 10\n|-\n| 1 || 15\n|}"
               {} {})
        ctx (make-ctx {:level 1
                       :table-cache page
                       :row {}})]
    (assert-eq (eval-node ctx [:dotref "Base Stats" "Damage"]) 15
               "dotref via page cache"))

  (suite "Electroshocker page: config + cache + CE via row numbers")
  (let [wiki (.. "<var>\n$CE$ = Total Price / DPS\n$DPS$ = Damage / Firerate\n$FNC-COST$ = 375; 200; 700\n</var>\n"
                 electroshocker-table)
        (cfg _) (parse-page-config wiki "")
        ast (parse-table electroshocker-table {})
        cache (build-table-cache ast "Level" {})
        row (cache-get cache 0 "")
        _ (set row.DPS (/ row.Damage row.Firerate))
        env cfg.formula-env
        (ce-ast _) (parse-with-env (. env "CE") env)
        ctx (make-ctx {:config cfg :level 0 :row row})]
    (assert-near (eval-node ctx ce-ast) (/ 375 (/ 2 0.75)) 1e-9
                 "CE at L0 from cached row"))

  true)

{:run run}
