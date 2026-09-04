;; structural wikitable (Harvester, Thorns || typo, Electroshocker)

(local {: suite : assert-eq : assert-true} (require :test_util))
(local {: parse-level-keys
        : clean-header-text
        : split-header-cells
        : split-row-cells
        : parse-table
        : find-table-blocks
        : parse-all-tables
        : detect-branch
        : index-col
        : row-level-keys} (require :wikitable))

(local electroshocker-table "{| class=\"wikitable stats-table\"
! Level !! Total Price !! Damage !! Firerate !! Range !! Chain Range !! Max Hits !! Shock Time !! Defense Drop !! DPS$DREF$ !! Max DPS$MREF$ !! Cost Efficiency
|-
| 0 || {{Money|$TP$}} || 2 || 0.75 || 11 || 7 || 1 || 0.05 || 5% || $DPS$ || $MDPS$ || {{Money|$CE$}}
|-
| 1 || {{Money|$TP$}} || 3 || 0.75 || 12.5 || 7 || 1 || 0.05 || 5% || $DPS$ || $MDPS$ || {{Money|$CE$}}
|-
| 5 || {{Money|$TP$}} || 200 || 2.25 || 17 || 10 || 3 || 0.3 || 25% || $DPS$ || $MDPS$ || {{Money|$CE$}}
|}")

(local harvester-table "{| class=\"wikitable stats-table\"
! colspan=\"8\" |Harvester Stats
|-
! Level !! Total Price !! Damage !! Firerate !! Range !! Projectile Speed !! DPS !! Cost Efficiency
|-
| 0 || {{Money|$TP$}} || 20 || 1.4 || 20 || 50 || $DPS$ || {{Money|$CE$}}
|-
| 1 || {{Money|$TP$}} || 20 || 1.2 || 20 || 50 || $DPS$ || {{Money|$CE$}}
|}")

;; Thorns used || before Cost Efficiency which used to merge last two cols
(local thorns-table "{| class=\"wikitable stats-table\"
! colspan=\"10\" |Thorns Stats
|-
! Level !! Total Price !! Thorns Damage !! Thorns Range !! Thorns Duration !! Thorns Slowdown !! Thorns Tick !! Total Elapsed Damage$TREF$ !! Thorns DPS$DREF$ || Cost Efficiency
|-
| 0 || {{Money|2,000}} || 2 || 12  || 8 || 20% || 0.25 || $TED$ || $TDPS$ || {{Money|$TCE$}}
|-
| 1 || {{Money|2,625}} || 4 || 12 || 8 || 20% || 0.25 || $TED$ || $TDPS$ || {{Money|$TCE$}}
|}")

(local harvester-page (.. "<var>\n$DPS$ = Damage / Firerate\n</var>\n"
                          "<div class=\"overflow-box\">\n" harvester-table
                          "\n</div>\n\n" "<div class=\"overflow-box\">\n"
                          thorns-table "\n</div>\n"))

(fn run []
  (suite "parse-level-keys singles and ranges")
  (assert-eq (parse-level-keys "3") [3] "single")
  (assert-eq (parse-level-keys "1-3") [1 2 3] "numeric range")
  (assert-eq (parse-level-keys "1/3") [1 3] "slash list")
  (assert-eq (parse-level-keys "2A") ["2A"] "branch key")
  (assert-eq (parse-level-keys "1A-3A") ["1A" "2A" "3A"] "branch range")
  (assert-eq (parse-level-keys "1A/3B") ["1A" "3B"] "mixed slash")
  (assert-eq (parse-level-keys "  2 A  ") ["2A"] "whitespace stripped")
  (assert-eq (parse-level-keys "") [] "empty")
  (suite "clean-header-text strips refs and vars")
  (assert-eq (clean-header-text "DPS$DREF$") "DPS" "var suffix")
  (assert-eq (clean-header-text "Thorns DPS$DREF$") "Thorns DPS" "multi-word")
  (assert-eq (clean-header-text "[[Splash Damage]]") "Splash Damage" "wikilink")
  (assert-eq (clean-header-text "Foo<ref>x</ref>") "Foo" "ref")
  (assert-eq (clean-header-text "[[File:Range Buff.png|16px]]Beams") "Beams"
             "file link is chrome")
  (assert-eq (clean-header-text "[[Image:QuestsIcon.png|12px]] Name") "Name"
             "image link is chrome")
  (suite "split-header-cells treats || as !!")
  (let [cells (split-header-cells "! Level !! Damage || Cost Efficiency")]
    (assert-eq cells ["Level" "Damage" "Cost Efficiency"]
               "|| typo yields three headers"))
  (suite "Thorns header || typo does not merge last columns")
  (let [t (parse-table thorns-table {})]
    (assert-eq t.title "Thorns Stats" "title")
    (assert-eq (length t.headers) 10 "10 headers including Cost Efficiency")
    (assert-eq (. t.headers 9) "Thorns DPS" "9th is Thorns DPS not merged")
    (assert-eq (. t.headers 10) "Cost Efficiency" "10th Cost Efficiency")
    (assert-eq (length t.rows) 2 "two data rows")
    (assert-eq (. t.rows 1 :level-raw) "0" "L0")
    (assert-eq (. t.rows 1 :by-header "Thorns Damage") "2" "by-header")
    (assert-eq (. t.rows 1 :by-header "Cost Efficiency") "{{Money|$TCE$}}"
               "last cell under correct header")
    (assert-eq (. t.rows 1 :by-header "Thorns DPS") "$TDPS$" "Thorns DPS cell"))
  (suite "Electroshocker table structure")
  (let [t (parse-table electroshocker-table {})]
    (assert-eq t.title nil "no colspan title")
    (assert-eq t.branch "" "no branch")
    (assert-eq (length t.headers) 12 "12 columns")
    (assert-eq (. t.headers 1) "Level" "Level")
    (assert-eq (. t.headers 10) "DPS" "DPS cleaned of $DREF$")
    (assert-eq (. t.headers 11) "Max DPS" "Max DPS")
    (assert-eq (length t.rows) 3 "3 rows in fixture")
    (assert-eq (. t.rows 1 :by-header "Damage") "2" "L0 damage")
    (assert-eq (. t.rows 3 :by-header "Damage") "200" "L5 damage")
    (assert-eq (. t.rows 1 :by-header "Firerate") "0.75" "firerate")
    (assert-true (: (. t.rows 1 :by-header "Total Price") :find "%$TP%$")
                 "TP var preserved raw"))
  (suite "Harvester titled table + branch map")
  (let [bmap {"Harvester Stats" "N"
              ; not a real branch letter usage
              "Thorns Stats" "T"
              "Top Path Stats" "A"}
        t (parse-table harvester-table bmap)]
    (assert-eq t.title "Harvester Stats" "title")
    (assert-eq t.branch "N" "branch from map")
    (assert-eq (length t.headers) 8 "8 cols")
    (assert-eq (. t.rows 1 :by-header "Damage") "20" "damage")
    (assert-eq (. t.rows 1 :by-header "DPS") "$DPS$" "formula cell raw"))
  (suite "detect-branch")
  (assert-eq (detect-branch "Top Path Stats" {"Top Path Stats" "A"}) "A"
             "exact")
  (assert-eq (detect-branch "TopPathStats"
                            {"Top Path Stats" "A" "TopPathStats" "A"})
             "A" "stripped key")
  (assert-eq (detect-branch "Nope" {"A" "A"}) "" "miss")
  (suite "find-table-blocks + parse-all-tables on Harvester page")
  (let [blocks (find-table-blocks harvester-page)
        tables (parse-all-tables harvester-page {"Thorns Stats" "X"})]
    (assert-eq (length blocks) 2 "two tables")
    (assert-eq (length tables) 2 "two parsed")
    (assert-eq (. tables 1 :title) "Harvester Stats" "first title")
    (assert-eq (. tables 2 :title) "Thorns Stats" "second title")
    (assert-eq (. tables 2 :branch) "X" "thorns branch")
    (assert-eq (length (. tables 2 :headers)) 10 "thorns headers fixed"))
  (suite "index-col and row-level-keys")
  (let [t (parse-table electroshocker-table {})
        col (index-col t nil)]
    (assert-eq col "Level" "default first header")
    (assert-eq (row-level-keys (. t.rows 1) col) [0] "L0 key")
    (assert-eq (row-level-keys (. t.rows 3) col) [5] "L5 key")
    (let [ov [{:table "Sentry Stats" :col "Alt"}]
          t2 (parse-table "{| \n! colspan=\"2\" |Sentry Stats\n|-\n! Level !! Alt\n|-\n| 1 || 2A\n|}"
                          {})]
      (assert-eq (index-col t2 ov) "Alt" "INDEX override")
      (assert-eq (row-level-keys (. t2.rows 1) "Alt") ["2A"] "branch key")))
  (suite "split-row-cells")
  (assert-eq (split-row-cells "| 0 || 1 || $DPS$") ["0" "1" "$DPS$"]
             "three cells")
  (suite "range level keys on a row")
  (let [t (parse-table "{|\n! Level !! Damage\n|-\n| 1-3 || 9\n|}" {})]
    (assert-eq (row-level-keys (. t.rows 1) "Level") [1 2 3] "1-3 expands"))
  true)

{:run run}
