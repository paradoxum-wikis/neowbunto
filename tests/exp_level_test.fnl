;; EXP / Level / TOTAL-EXP via #expr

(local {: suite : assert-eq : assert-true} (require :test_util))
(local {: parse-var : parse-var-env : mw-expr-body? : extract-mw-expr}
       (require :parser))

(local {: eval-node : make-ctx : sum-total : expr-bindings} (require :eval))
(local mwexpr (require :mwexpr))
(local {: render-page} (require :render))

(local EXP-BODY "floor(50 * 1.09^(Level - 1) * (Level > 0))")
(local EXP-RAW (.. "{{#expr:" EXP-BODY "}}"))

(local scout-levels (.. "<var>
$DPS$ = Damage / Firerate
$TP$ = $FNC-TOTALPRICE$
$TE$ = $FNC-TOTAL-EXP$
$EXP$ = " EXP-RAW "
$FNC-COST$ = 125; 50; 375; 1350; 2200
$R$ = $FNC-RECURSION$
</var>
{| class=\"wikitable stats-table\"
! colspan=\"12\" |Levels
|-
! Level !! Exp !! Total Exp
|-
| 0 || {{Exp|0}} || {{Exp|$TE$}}$R$
|-
| 1 || {{Exp|$EXP$}}$R$ || $R$
|-
| 2 || $R$ || $R$
|-
| 3 || $R$ || $R$
|-
| 4 || $R$ || $R$
|}
"))

(fn run []
  (suite "classify: #expr is a parser function, not arithmetic")
  (assert-true (mw-expr-body? EXP-RAW) "whole value is {{#expr:...}}")
  (assert-eq (extract-mw-expr EXP-RAW) EXP-BODY "strips wrapper")
  (assert-true (not (mw-expr-body? "Damage / Firerate")) "plain formula")
  (assert-true (not (mw-expr-body? "{{Exp|1}}")) "template is not #expr")
  (suite "mwexpr: MediaWiki #expr language")
  (assert-eq (mwexpr.eval "1+2*3" {}) 7 "precedence")
  (assert-eq (mwexpr.eval "2^3^2" {}) 512 "power right-assoc (2^(3^2))")
  (assert-eq (mwexpr.eval "floor(50 * 1.09^(1 - 1) * (1 > 0))" {}) 50 "L1 nums")
  (assert-eq (mwexpr.eval "floor(50 * 1.09^(0 - 1) * (0 > 0))" {}) 0 "L0 nums")
  (assert-eq (mwexpr.eval "floor(50 * 1.09^(2 - 1) * (2 > 0))" {}) 54 "L2 nums")
  (assert-eq (mwexpr.eval EXP-BODY {"Level" 1}) 50 "L1 via binding")
  (assert-eq (mwexpr.eval EXP-BODY {"Level" 0}) 0 "L0 via binding")
  (assert-eq (mwexpr.eval EXP-BODY {"Level" 3}) 59 "L3 via binding")
  (assert-eq (mwexpr.eval-wrapped EXP-RAW {"Level" 2}) 54 "wrapped form")
  ;; MediaWiki infix round (Bomb Damage * 0.3 round 0)
  (assert-eq (mwexpr.eval "10.7 round 0" {}) 11 "round 0")
  (assert-eq (mwexpr.eval "2 * 3.5 round 0" {}) 7 "round after *")
  (assert-eq (mwexpr.eval "1.234 round 2" {}) 1.23 "round 2")
  (suite "parse EXP -> [:mw-expr body], eval with ctx.level")
  (let [env {"EXP" EXP-RAW}
        cache {}
        ast (parse-var "EXP" env cache [])]
    (assert-eq (. ast 1) :mw-expr "node tag")
    (assert-eq (. ast 2) EXP-BODY "body kept")
    (assert-eq (eval-node (make-ctx {:level 0 :row {}}) ast) 0 "L0")
    (assert-eq (eval-node (make-ctx {:level 1 :row {}}) ast) 50 "L1")
    (assert-eq (eval-node (make-ctx {:level 2 :row {}}) ast) 54 "L2")
    (assert-eq (eval-node (make-ctx {:level 3 :row {"Level" 99}}) ast) 59
               "ctx.level wins over stale row Level for binding"))
  (suite "frame:callParserFunction{ name='#expr', args={expr} }")
  (let [env {"EXP" EXP-RAW}
        ast (parse-var "EXP" env {} [])
        calls []
        frame (mw._makeFrame {})
        real-cpf frame.callParserFunction]
    (set frame.callParserFunction
         (fn [self a b ...]
           (table.insert calls [a b])
           (real-cpf self a b ...)))
    (assert-eq (eval-node (make-ctx {:level 1 :row {} :frame frame}) ast) 50
               "via callParserFunction")
    (assert-true (> (length calls) 0) "invoked")
    (let [kwargs (. calls 1 1)]
      (assert-eq (type kwargs) :table "kwargs table")
      (assert-eq kwargs.name "#expr" "name = #expr")
      (assert-eq (type kwargs.args) :table "args table")
      (let [expr (. kwargs.args 1)]
        (assert-true (not (expr:find "Level" 1 true))
                     "Level already materialized")
        (assert-true (expr:find "1" 1 true) "numeric level in expr"))))
  (suite "expr-bindings: Level from ctx")
  (let [b (expr-bindings {:level 4 :row {"Damage" 10}})]
    (assert-eq b.Level 4 "Level")
    (assert-eq b.Damage 10 "row field"))
  (suite "TOTAL-EXP series matches Scout Levels table")
  ;; Scout Levels TE: L0=0, L1=50, L2=104, L3=163, L4=227
  (let [env {"EXP" EXP-RAW "TE" "$FNC-TOTAL-EXP$"}
        cache (parse-var-env env)
        vars {"$EXP$" EXP-RAW}
        cfg {:vars vars :formula-env env}]
    (each [_ [lvl expected] (ipairs [[0 0] [1 50] [2 104] [3 163] [4 227]])]
      (let [ctx (make-ctx {:config cfg
                           :vars vars
                           :formula-env env
                           :parse-cache cache
                           :formula-asts cache
                           :level lvl
                           :row {"Level" lvl}})]
        (assert-eq (sum-total ctx "EXP") expected
                   (.. "sum EXP 0.." (tostring lvl))))))
  (suite "TE alias -> intrinsic TOTAL-EXP")
  (let [env {"EXP" EXP-RAW "TE" "$FNC-TOTAL-EXP$"}
        cache (parse-var-env env)
        vars {"$EXP$" EXP-RAW}
        ctx (make-ctx {:level 2
                       :row {"Level" 2}
                       :vars vars
                       :formula-env env
                       :parse-cache cache
                       :formula-asts cache
                       :config {:vars vars :formula-env env}})]
    (assert-eq (eval-node ctx (. cache "TE")) 104 "TE@2"))
  (suite "render Levels table (recursion + EXP + TE)")
  (let [out (render-page scout-levels nil {:seed 1})]
    (assert-true (not (out:find "%$EXP%$")) "no raw $EXP$")
    (assert-true (not (out:find "%$TE%$")) "no raw $TE$")
    (assert-true (out:find "{{Exp|50}}" 1 true) "L1 exp")
    (assert-true (out:find "{{Exp|54}}" 1 true) "L2 exp")
    (assert-true (out:find "{{Exp|104}}" 1 true) "L2 total")
    (assert-true (out:find "{{Exp|0}}" 1 true) "L0")
    (assert-true (out:find "{{Exp|227}}" 1 true) "L4 total"))
  (suite "inline #expr in a cell materializes Table.Col")
  (let [src "<var>
$4BP$ = Operator Stats.Coordination Damage Boost * 4
</var>
{| class=\"wikitable\"
! colspan=\"4\" | Operator Stats
|-
! Level !! Damage !! Coordination Damage Boost
|-
| 2 || 6 || 10%
|}
{| class=\"wikitable\"
! colspan=\"3\" | Coordination Stats
|-
! Level !! Boost Pct !! Boost Damage
|-
| 2 || $4BP$ || {{#expr:floor(Operator Stats.Damage * (1 + Operator Stats.Coordination Damage Boost * 4 * 0.01))}}
|}
"
        out (render-page src nil {:seed 1})]
    (assert-true (not (out:find "Unrecognized" 1 true)) "no expr error")
    (assert-true (not (out:find "{{#expr:" 1 true)) "expr expanded")
    (assert-true (out:find "40" 1 true) "10% * 4")
    (assert-true (out:find "8" 1 true) "floor(6*1.4)"))
  true)

{:run run}
