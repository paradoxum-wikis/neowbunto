;; heeho entry plumbing

(local {: suite : assert-eq : assert-true} (require :test_util))
(local {: heeho : content-arg} (require :init))

(fn mock-frame [arg1]
  (let [frame (mw._makeFrame {1 arg1})
        real-pre frame.preprocess]
    ;; stamp preprocess so heeho tests can assert it ran
    (set frame.preprocess
         (fn [self s]
           (.. "PRE:" (real-pre self s))))
    frame))

(fn run []
  (suite "content-arg reads invoke #1 (Template:Neow pattern)")
  (assert-eq (content-arg (mock-frame "hello")) "hello" "plain")
  (assert-eq (content-arg (mock-frame "  padded  ")) "padded" "trimmed")
  (assert-eq (content-arg (mock-frame "   ")) "" "whitespace -> empty")
  (assert-eq (content-arg (mock-frame nil)) "" "nil")
  (assert-eq (content-arg {:args {}}) "" "missing #1")
  (assert-eq (content-arg nil) "" "nil frame")
  ;; parent frame ignored
  ;; template already put body in invoke #1
  (let [frame {:args {1 "from-invoke"}
               :getParent (fn []
                            {:args {1 "from-parent"}})}]
    (assert-eq (content-arg frame) "from-invoke" "invoke wins; parent unused"))
  (suite "heeho empty / missing content")
  (let [msg "'''Neowbunto''': No valid content found."]
    (assert-eq (heeho (mock-frame nil)) msg "nil arg")
    (assert-eq (heeho (mock-frame "")) msg "empty string")
    (assert-eq (heeho (mock-frame "  \n\t  ")) msg "whitespace only")
    (assert-eq (heeho {:args {}}) msg "no arg1"))
  (suite "heeho unescapes HTML entities")
  (let [raw "&lt;var&gt;\n$X$ = 1\n&lt;/var&gt;\nThe number is $X$."
        out (heeho (mock-frame raw))]
    (assert-true (out:find "PRE:" 1 true) "preprocess called")
    (assert-true (out:find "1" 1 true) "value expanded")
    (assert-true (not (out:find "%$X%$")) "token gone"))
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
        out (heeho (mock-frame wiki))]
    (assert-true (out:find "PRE:" 1 true) "preprocess")
    (assert-true (not (out:find "<var>" 1 true)) "var stripped")
    (assert-true (or (out:find "14.29" 1 true) (out:find "14.285" 1 true)
                     (out:find "14.2" 1 true)) "DPS expanded")
    (assert-true (out:find "20" 1 true) "damage kept"))
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
        out (heeho (mock-frame wiki))]
    (assert-true (out:find "rofbug-wrapper" 1 true) "wrapper")
    (assert-true (out:find "wds-button" 1 true) "button")
    (assert-true (out:find "Disable Rate of Fire Bug" 1 true) "label")
    (assert-true (out:find "PRE:" 1 true) "preprocess"))
  (suite "heeho FSE path includes open-se button")
  (let [wiki "<var>
$FSE-CATEGORY$ = Custom
$FNC-COST$ = 1
</var>
Hello $FSE-CATEGORY$."
        out (heeho (mock-frame wiki))]
    (assert-true (out:find "open-se" 1 true) "editor button")
    (assert-true (out:find "Open in Statistics Editor" 1 true) "label")
    (assert-true (out:find "open%-se.-</div>.-Hello") "content outside wrapper"))
  (suite "heeho unstripNoWiki is used")
  (let [orig mw.text.unstripNoWiki
        seen []]
    (set mw.text.unstripNoWiki (fn [s]
                                 (table.insert seen s)
                                 s))
    (heeho (mock-frame "<var>\n$A$ = 2\n</var>\n$A$"))
    (set mw.text.unstripNoWiki orig)
    (assert-eq (length seen) 1 "unstrip called once")
    (assert-true (: (. seen 1) :find "%$A%$") "passed raw content"))
  true)

{:run run}
