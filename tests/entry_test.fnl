;; p.heeho entry plumbing

(local {: suite : assert-eq : assert-true} (require :test_util))
(local p (require :init))

(fn mock-frame [arg1]
  (let [frame (mw._makeFrame {1 arg1})]
    ;; stamp preprocess so heeho tests can assert it ran
    (let [real-pre frame.preprocess]
      (set frame.preprocess
           (fn [self s]
             (.. "PRE:" (real-pre self s)))))
    frame))

(fn run []
  (suite "content-arg reads invoke #1 (Template:Neow pattern)")
  (assert-eq (p.content-arg (mock-frame "hello")) "hello" "plain")
  (assert-eq (p.content-arg (mock-frame "  padded  ")) "padded" "trimmed")
  (assert-eq (p.content-arg (mock-frame "   ")) "" "whitespace -> empty")
  (assert-eq (p.content-arg (mock-frame nil)) "" "nil")
  (assert-eq (p.content-arg {:args {}}) "" "missing #1")
  (assert-eq (p.content-arg nil) "" "nil frame")
  ;; parent frame ignored; template already put body in invoke #1
  (assert-eq (p.content-arg {:args {1 "from-invoke"}
                             :getParent (fn []
                                          {:args {1 "from-parent"}})})
             "from-invoke"
             "invoke wins; parent unused")

  (suite "heeho empty / missing content")
  (assert-eq (p.heeho (mock-frame nil))
             "'''Neowbunto''': No valid content found."
             "nil arg")
  (assert-eq (p.heeho (mock-frame ""))
             "'''Neowbunto''': No valid content found."
             "empty string")
  (assert-eq (p.heeho (mock-frame "  \n\t  "))
             "'''Neowbunto''': No valid content found."
             "whitespace only")
  (assert-eq (p.heeho {:args {}})
             "'''Neowbunto''': No valid content found."
             "no arg1")

  (suite "heeho unescapes HTML entities")
  (let [out (p.heeho (mock-frame
                       "&lt;var&gt;\n$X$ = 1\n&lt;/var&gt;\nThe number is $X$."))]
    (assert-true (: out :find "PRE:" 1 true) "preprocess called")
    (assert-true (: out :find "1" 1 true) "value expanded")
    (assert-true (not (: out :find "%$X%$")) "token gone"))

  (suite "heeho strips var block and expands formula")
  (let [wiki "<var>
$DPS$ = Damage / Firerate
$FNC-COST$ = 100
</var>
{| class=\"wikitable\"
! Level !! Damage !! Firerate !! DPS
|-
| 0 || 20 || 1.4 || $DPS$
|}"
        out (p.heeho (mock-frame wiki))]
    (assert-true (: out :find "PRE:" 1 true) "preprocess")
    (assert-true (not (: out :find "<var>" 1 true)) "var stripped")
    (assert-true (or (: out :find "14.29" 1 true)
                     (: out :find "14.285" 1 true)
                     (: out :find "14.2" 1 true))
                 "DPS expanded")
    (assert-true (: out :find "20" 1 true) "damage kept"))

  (suite "heeho ROF path returns wrapper")
  (let [wiki "<var>
$FNC-ROFBUG$ = Firerate
$FNC-COST$ = 1
</var>
{| class=\"wikitable\"
! Level !! Firerate
|-
| 0 || 0.75
|}"
        out (p.heeho (mock-frame wiki))]
    (assert-true (: out :find "rofbug-wrapper" 1 true) "wrapper")
    (assert-true (: out :find "wds-button" 1 true) "button")
    (assert-true (: out :find "Disable Rate of Fire Bug" 1 true) "label")
    (assert-true (: out :find "PRE:" 1 true) "preprocess"))

  (suite "heeho FSE path includes open-se button")
  (let [wiki "<var>
$FSE-CATEGORY$ = Custom
$FNC-COST$ = 1
</var>
Hello $FSE-CATEGORY$."
        out (p.heeho (mock-frame wiki))]
    (assert-true (: out :find "open-se" 1 true) "editor button")
    (assert-true (: out :find "Open in Statistics Editor" 1 true) "label")
    (assert-true (: out :find "open%-se.-</div>.-Hello") "content outside wrapper"))

  (suite "heeho unstripNoWiki is used")
  (let [orig mw.text.unstripNoWiki
        seen []]
    (set mw.text.unstripNoWiki
         (fn [s]
           (table.insert seen s)
           s))
    (p.heeho (mock-frame "<var>\n$A$ = 2\n</var>\n$A$"))
    (set mw.text.unstripNoWiki orig)
    (assert-eq (length seen) 1 "unstrip called once")
    (assert-true (: (. seen 1) :find "%$A%$") "passed raw content"))

  true)

{:run run}
