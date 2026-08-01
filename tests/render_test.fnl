;; ROF chrome, refs, nested $DPSREF$ footnotes

(local {: suite : assert-eq : assert-true} (require :test_util))
(local {: new-toggle-builder : next-cell-toggle! : rof-cell-html
        : make-editor-btn-html : wrap-rofbug : build-out
        : strip-var-blocks : render-page : fmt}
       (require :render))
(local {: parse-page-config} (require :config))

(fn first-leftover-var [s]
  ;; catches $DPSGUN$ still sitting in footnotes
  (s:match "%$([A-Za-z][%w%-]*)%$"))

(fn assert-no-leftover-vars [out label]
  (let [name (first-leftover-var out)]
    (assert-true (= name nil)
                 (.. (or label "no leftover $VAR$")
                     (if name (.. " (found $" name "$)") "")))))

(fn run []
  (suite "fmt strips trailing zeros")
  (assert-eq (fmt 1.5 2) "1.5" "1.5")
  (assert-eq (fmt 2 2) "2" "2")
  (assert-eq (fmt "x") "x" "passthrough")

  (suite "toggle builder allocates cell ids and classes")
  (let [b (new-toggle-builder 42)
        id1 (next-cell-toggle! b)
        id2 (next-cell-toggle! b)]
    (assert-true (b.id:find "^rofbug%-") "id prefix")
    (assert-eq id1 (.. b.id "-c0") "first cell")
    (assert-eq id2 (.. b.id "-c1") "second cell")
    (assert-eq (length b.classes) 4 "two pairs of classes")
    (assert-true (: (. b.classes 1) :find "mw-customtoggle-" 1 true)
                 "customtoggle class"))

  (suite "rof-cell-html collapsible pair (off collapsed by default)")
  (let [html (rof-cell-html "rofbug-1-c0" "0.75" "0.8")]
    (assert-true (html:find "mw-collapsible mw-collapsed" 1 true) "collapsed off")
    (assert-true (html:find "id=\"mw-customcollapsible-rofbug-1-c0-off\"" 1 true)
                 "off id")
    (assert-true (html:find "id=\"mw-customcollapsible-rofbug-1-c0-on\"" 1 true)
                 "on id")
    (assert-true (html:find ">0.75<" 1 true) "norm value")
    (assert-true (html:find ">0.8<" 1 true) "rof value")
    ;; off span collapsed = normal firerate when ROF off
    (assert-true (html:find
                   "class=\"mw-collapsible mw-collapsed\" id=\"mw-customcollapsible-rofbug-1-c0-off\""
                   1 true)
                 "off is collapsed by default"))

  (suite "wrap-rofbug button structure for wiki CSS")
  (let [b (new-toggle-builder 1)
        _ (next-cell-toggle! b)
        html (wrap-rofbug "| table |" b false)]
    (assert-true (html:find "class=\"rofbug-wrapper\"" 1 true) "wrapper")
    ;; ROF is .wds-button not .open-se
    (assert-true (html:find "class=\"wds-button " 1 true) "wds-button")
    (assert-true (not (html:find "wds-button open-se" 1 true))
                 "no open-se without FSE")
    (assert-true (html:find "Disable Rate of Fire Bug" 1 true) "disable label")
    (assert-true (html:find "Enable Rate of Fire Bug" 1 true) "enable label")
    (assert-true (html:find "mw-collapsed" 1 true) "enable starts collapsed")
    (assert-true (html:find "mw-customtoggle-" 1 true) "label toggles")
    (assert-true (html:find (.. "mw-customtoggle-" b.id "-c0-off") 1 true)
                 "cell toggle on button")
    (assert-true (html:find ">| table |<" 1 true) "table body"))

  (suite "wrap-rofbug with FSE adds .wds-button.open-se sibling")
  (let [b (new-toggle-builder 2)
        html (wrap-rofbug "X" b true)]
    (assert-true (html:find "class=\"wds-button open-se\"" 1 true)
                 "open-se sibling after ROF btn")
    (assert-true (html:find "Open in Statistics Editor" 1 true) "editor label")
    (assert-true (html:find "se.tds.wiki/tower/" 1 true) "editor url")
    (let [i-btn (html:find "class=\"wds-button " 1 true)
          i-se (html:find "class=\"wds-button open-se\"" 1 true)]
      (assert-true (and i-btn i-se (< i-btn i-se))
                   "ROF button before open-se")))

  (suite "FSE-only: wrapper is button-only, content is sibling")
  (let [out (render-page
              "<var>\n$FSE-CATEGORY$ = X\n$FNC-COST$ = 1\n</var>\nBODY"
              nil {})]
    (assert-true (out:find "rofbug-wrapper" 1 true) "has wrapper")
    (assert-true (out:find "open-se" 1 true) "has open-se")
    ;; wrapper then BODY sibling not wrapping BODY
    (assert-true (out:find "</div>\nBODY" 1 true)
                 "BODY after closed wrapper"))

  (suite "/History never gets Open in Statistics Editor (FSE or not)")
  (let [title mw.title.getCurrentTitle
        _ (set mw.title.getCurrentTitle (fn [] {:text "Juggernaut/History"}))
        out (render-page
              "<var>\n$FSE-CATEGORY$ = X\n$FNC-COST$ = 1\n</var>\nBODY"
              nil {})]
    (set mw.title.getCurrentTitle title)
    (assert-true (not (out:find "open-se" 1 true)) "no open-se")
    (assert-true (not (out:find "Open in Statistics Editor" 1 true))
                 "no editor label")
    ;; empty wrapper shell still present
    ;; BODY is sibling
    (assert-true (out:find "<div class=\"rofbug-wrapper\"></div>" 1 true)
                 "empty wrapper shell")
    (assert-true (out:find "</div>\nBODY" 1 true) "BODY sibling"))

  (suite "make-editor-btn-html alone")
  (assert-eq (make-editor-btn-html false) nil "no fse")
  (assert-true (: (make-editor-btn-html true) :find "open-se" 1 true) "fse")

  (suite "process-table ROF collapsible on Firerate column")
  (let [wiki "<var>
$FNC-ROFBUG$ = Firerate
$FNC-COST$ = 100
</var>
{| class=\"wikitable\"
! Level !! Firerate !! Damage
|-
| 0 || 0.75 || 2
|-
| 1 || 0.65 || 3
|}"
        (cfg _) (parse-page-config wiki "")
        body (strip-var-blocks wiki)
        builder (new-toggle-builder 99)
        out (build-out body cfg.vars cfg nil builder)]
    (assert-true (out:find "mw-customcollapsible-" 1 true) "has collapsibles")
    (assert-true (out:find "0.75" 1 true) "norm firerate present")
    (assert-true (> builder.cell-count 0) "cells toggled")
    (assert-true (out:find "||2" 1 true) "damage raw-ish"))

  (suite "render-page wraps when ROFBUG present")
  (let [wiki "<var>
$FNC-ROFBUG$ = Firerate
$FNC-COST$ = 375
$DPS$ = Damage / Firerate
</var>
{| class=\"wikitable\"
! Level !! Firerate !! Damage !! DPS
|-
| 0 || 0.75 || 2 || $DPS$
|}"
        html (render-page wiki nil {:seed 7})]
    (assert-true (html:find "rofbug-wrapper" 1 true) "wrapper")
    (assert-true (html:find "wds-button" 1 true) "button")
    (assert-true (html:find "Disable Rate of Fire Bug" 1 true) "label")
    (assert-true (html:find "mw-collapsible" 1 true) "cell or label collapsible"))

  (suite "render-page without ROF is plain tables")
  (let [wiki "<var>
$FNC-COST$ = 1
$DPS$ = Damage / Firerate
</var>
{| class=\"wikitable\"
! Level !! Damage !! Firerate !! DPS
|-
| 0 || 20 || 1.4 || $DPS$
|}"
        out (render-page wiki nil {:seed 1})]
    (assert-true (not (out:find "rofbug-wrapper" 1 true)) "no wrapper")
    (assert-true (out:find "20" 1 true) "damage")
    (assert-true (or (out:find "14.29" 1 true)
                     (out:find "14.285" 1 true)
                     (out:find "14.2" 1 true))
                 "DPS expanded"))

  (suite "free-text $VAR$ outside tables (HelpNeowtext EQ)")
  (let [wiki "<var>
$EQ$ = 5 + 5 - 2**5
</var>
5 + 5 - 2^5 is $EQ$!"
        out (render-page wiki nil {})]
    (assert-true (out:find "is -22!" 1 true) "EQ expanded in prose")
    (assert-true (not (out:find "%$EQ%$")) "no raw $EQ$"))

  (suite "ref dedup: first full, later self-closing")
  (let [wiki "<var>
$DREF$ = <ref name=\"dpsnote\">Assumes one enemy.</ref>
$FNC-COST$ = 1
</var>
Text $DREF$ and again $DREF$."
        out (render-page wiki nil {})]
    (assert-true (out:find "<ref name=\"dpsnote\">Assumes one enemy.</ref>" 1 true)
                 "first defining ref")
    (assert-true (out:find "<ref name=\"dpsnote\"/>" 1 true)
                 "second self-closing")
    (assert-true (not (out:find "%$DREF%$")) "no raw token left"))

  (suite "nested $VAR$ inside $DPSREF$ fully evaluates (not tokens/formula text)")
  (let [wiki "<var>
$DPS$ = $DPSGUN$ + $DPSPOISON$
$DPSGUN$ = (Damage * Burst Count) / (Cooldown + (Firerate * Burst Count))
$DPSPOISON$ = (Poison Damage / Tick)
$DPSREF$ = <ref>$DPSGUN$ Gun DPS + $DPSPOISON$ Poison DPS</ref>
$FNC-COST$ = 525; 200; 800
</var>
{| class=\"wikitable\"
! Level !! Damage !! Poison Damage !! Burst Count !! Firerate !! Tick !! Cooldown !! DPS
|-
| 0 || 1 || 1 || 4 || 0.12 || 1 || 1.2 || $DPS$$DPSREF$
|-
| 1 || 1 || 1 || 4 || 0.12 || 1 || 0.6 || $DPS$$DPSREF$
|}"
        out (render-page wiki nil {:seed 3})
        has-num-ref (or (out:find "2.38 Gun DPS + 1 Poison DPS" 1 true)
                        (out:find "2.381 Gun DPS + 1 Poison DPS" 1 true))
        has-l1 (or (out:find "3.7 Gun DPS + 1 Poison DPS" 1 true)
                   (out:find "3.70 Gun DPS + 1 Poison DPS" 1 true))]
    (assert-no-leftover-vars out "nested DPSREF")
    (assert-true (not (out:find "(Damage" 1 true)) "no bare Damage in output")
    (assert-true (not (out:find "(1 * 4)" 1 true)) "no formula-text footprint")
    (assert-true has-num-ref "L0 footnote fully numeric")
    (assert-true has-l1 "L1 footnote fully numeric")
    (assert-true (or (out:find "||3.38" 1 true) (out:find "3.38" 1 true))
                 "L0 total DPS ≈ 3.38"))

  ;; inline ref path not only the $DPSREF$ recurse path
  (suite "inline <ref>$DPSGUN$...</ref> fully evaluates (source4 first era)")
  (let [wiki "<var>
$DPS$ = $DPSGUN$ + $DPSPOISON$
$DPSGUN$ = (Damage * Burst Count) / (Cooldown + (Firerate * Burst Count))
$DPSPOISON$ = (Poison Damage / Tick)
$FNC-COST$ = 525
</var>
{| class=\"wikitable\"
! Level !! Damage !! Poison Damage !! Burst Count !! Firerate !! Tick !! Cooldown !! DPS
|-
| 0 || 1 || 1 || 4 || 0.12 || 1 || 1.2 || $DPS$<ref>$DPSGUN$ Gun DPS + $DPSPOISON$ Poison DPS</ref>
|}"
        out (render-page wiki nil {:seed 5})
        has-ref (or (out:find "2.38 Gun DPS + 1 Poison DPS" 1 true)
                    (out:find "2.381 Gun DPS + 1 Poison DPS" 1 true))]
    (assert-no-leftover-vars out "inline ref")
    (assert-true (out:find "<ref>" 1 true) "ref tag kept")
    (assert-true has-ref "inline footnote fully numeric")
    (assert-true (or (out:find "3.38" 1 true) (out:find "||3.38" 1 true))
                 "total DPS numeric"))

  ;; ROF dual path: 0.1 vs 0.125 firerate both fully numeric footnotes
  (suite "ROF + nested $DPSREF$ expands both collapsible footnote numbers")
  (let [wiki "<var>
$FNC-ROFBUG$ = Firerate
$DPS$ = $DPSGUN$ + $DPSPOISON$
$DPSGUN$ = (Damage * Burst Count) / (Cooldown + (Firerate * Burst Count))
$DPSPOISON$ = (Poison Damage / Tick)
$DPSREF$ = <ref>$DPSGUN$ Gun DPS + $DPSPOISON$ Poison DPS</ref>
$FNC-COST$ = 525; 200
</var>
{| class=\"wikitable\"
! Level !! Damage !! Poison Damage !! Burst Count !! Firerate !! Tick !! Cooldown !! DPS
|-
| 0 || 1 || 1 || 4 || 0.1 || 1 || 1.2 || $DPS$$DPSREF$
|}"
        out (render-page wiki nil {:seed 11})
        has-norm (out:find "2.5 Gun DPS + 1 Poison DPS" 1 true)
        has-rof (or (out:find "2.35 Gun DPS + 1 Poison DPS" 1 true)
                    (out:find "2.353 Gun DPS + 1 Poison DPS" 1 true))]
    (assert-true (out:find "rofbug-wrapper" 1 true) "ROF wrapper")
    (assert-true (out:find "mw-collapsible" 1 true) "collapsible cells")
    (assert-no-leftover-vars out "ROF nested DPSREF")
    (assert-true (not (out:find "(1 * 4)" 1 true)) "no half-subbed formula text")
    (assert-true has-norm "norm-path footnote 2.5 Gun DPS")
    (assert-true has-rof "ROF-path footnote ~2.35 Gun DPS")
    (assert-true (out:find "3.5" 1 true) "norm total DPS 3.5")
    (assert-true (or (out:find "3.35" 1 true) (out:find "3.353" 1 true))
                 "ROF total DPS ~3.35"))

  (suite "strip-var-blocks removes var section")
  (assert-eq (strip-var-blocks "<var>\n$X$ = 1\n</var>\n{| table")
             "{| table"
             "stripped")

  true)

{:run run}
