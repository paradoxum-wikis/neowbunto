;; The lovely AST evaluator

(local config (require :config))
(local mwexpr (require :mwexpr))
(local parser (require :parser))

;; ties by name so pairs order cannot change output
(fn longer-first? [a b]
  (let [la (length a)
        lb (length b)]
    (or (> la lb) (and (= la lb) (< a b)))))

(local expr-memos (setmetatable {} {:__mode :k}))

(fn extract-number [v]
  ;; numbers, "1,000", {{Money|7500}} scrape, N/A -> 0 (stats tables)
  (if (= (type v) :number)
      v
      (let [str (pick-values 1 (: (tostring (or v "")) :match "^%s*(.-)%s*$"))
            lower (str:lower)]
        (if (or (= lower "n/a") (= lower "na") (= lower "-") (= str ""))
            0
            (let [(cleaned) (str:gsub "," "")
                  n (tonumber cleaned)]
              (if (not= n nil)
                  n
                  (let [m (cleaned:match "%-?%d+%.?%d*")]
                    (and m (tonumber m)))))))))

(fn as-number [v label]
  (let [n (extract-number v)]
    (when (= n nil)
      (error (.. "eval: not a number for '" (or label "?") "': " (tostring v))))
    n))

(fn unresolved [ctx name]
  ;; missing keys hard error (NS0)
  (error (if (and ctx ctx.formula-name)
             (.. "unresolved identifier '" name "' in formula for $"
                 ctx.formula-name "$")
             (.. "unresolved identifier '" name "'"))))

(var eval-node nil)

(fn eval-named-formula [ctx name]
  ;; leftover $NAME$ in a cache cell, not a bare ident
  (let [env (or ctx.formula-env (and ctx.config ctx.config.formula-env))
        raw (and env (. env name))]
    (when (and raw (not= raw ""))
      (let [stack (or ctx.ident-stack {})]
        (when (. stack name)
          (error (.. "eval: cyclic formula '" name "'")))
        (set (. stack name) true)
        (set ctx.ident-stack stack)
        ;; pcall so cycle flag clears even when eval errors
        (let [asts (or ctx.formula-asts ctx.parse-cache)
              ast (or (and asts (. asts name))
                      (parser.parse-var name env (or ctx.parse-cache {}) []))
              (ok res) (pcall eval-node ctx ast)]
          (set (. stack name) nil)
          (if ok res (error res 0)))))))

(fn cell-formula-name [v]
  ;; $NAME$ still sitting in a cached cell (maybe glued $DPS$$RREF$)
  (when (and (= (type v) :string) (v:find "$" 1 true))
    (var s v)
    (when (v:find "<ref" 1 true)
      (set s (pick-values 1 (v:gsub "<ref[^>]*>.-</ref>" "")))
      (set s (pick-values 1 (s:gsub "<ref[^>]*/>" ""))))
    (s:match "%$([A-Za-z][%w%-]*)%$")))

(fn with-row! [ctx row f a b c]
  ;; a b c optional
  ;; pcall(f, ...) avoids a fresh closure
  (let [saved ctx.row]
    (set ctx.row row)
    (let [(ok res) (pcall f a b c)]
      (set ctx.row saved)
      (if ok res (error res 0)))))

(fn resolve-cell-value [ctx row v label]
  ;; row-lookup already handled nil and numbers
  (let [n (extract-number v)]
    (if (not= n nil)
        n
        (let [fname (cell-formula-name v)]
          (if fname
              (with-row! ctx row eval-named-formula ctx fname)
              (as-number v label))))))

(fn row-field [row name rof?]
  ;; cache rows keep Header_ROF
  ;; live row-rof mutates in place
  (if rof?
      (let [rv (. row (.. name "_ROF"))]
        (if (not= rv nil) rv (. row name)))
      (. row name)))

(fn row-lookup [ctx name]
  ;; exact key then stripped dual-write
  ;; still-$token$ is eval'd
  ;; missing is nil, not $VAR$
  (let [row (or ctx.row {})
        rof? ctx.rof?
        direct (row-field row name rof?)
        v (if (not= direct nil)
              direct
              (when (name:find "%s")
                (row-field row (pick-values 1 (name:gsub "%s+" "")) rof?)))]
    ;; nil = unresolvable as callers hard-error or soft-ident
    (if (= v nil) (when (= name "Level") ctx.level)
        (= (type v) :number) v
        (resolve-cell-value ctx row v name))))

(fn cache-get [cache level branch]
  (when cache
    (if (and branch (not= branch ""))
        (or (. cache (.. (tostring level) branch)) (. cache level))
        (. cache level))))

(fn table-lookup [ctx tname col]
  (let [tc ctx.table-cache
        entry (and tc (or (. tc tname) (. tc (tname:gsub "%s+" ""))))
        row (cache-get entry ctx.level ctx.branch)]
    (if (not row)
        nil
        (with-row! ctx row row-lookup ctx col))))

(fn array-index [level branch schema]
  ;; same (branch, level) -> flat walk as sum-series
  (let [lvl (or (tonumber level) 0)]
    (if (not schema)
        (when (>= lvl 0) (+ lvl 1))
        (let [trunk (or (. schema 1) "N")
              target (if (and branch (not= branch "")) branch trunk)]
          (var found nil)
          (var trunk-lvl 0)
          (local branch-lvls {})
          (each [i letter (ipairs schema) &until found]
            (if (= letter trunk)
                (do
                  (when (and (= target trunk) (= trunk-lvl lvl))
                    (set found i))
                  (set trunk-lvl (+ trunk-lvl 1)))
                (do
                  (when (not (. branch-lvls letter))
                    (set (. branch-lvls letter) trunk-lvl))
                  (when (and (= target letter) (= (. branch-lvls letter) lvl))
                    (set found i))
                  (set (. branch-lvls letter) (+ (. branch-lvls letter) 1)))))
          found))))

(fn sum-series [costs level branch schema]
  (let [lvl (or (tonumber level) 0)]
    (if (not schema)
        (let [cs (or costs [])]
          (var total 0)
          (var i 1)
          (while (and (not= (. cs i) nil) (<= (- i 1) lvl))
            (set total (+ total (. cs i)))
            (set i (+ i 1)))
          total)
        (let [trunk (or (. schema 1) "N")
              target (if (and branch (not= branch "")) branch trunk)]
          (var total 0)
          (var trunk-lvl 0)
          (local branch-lvls {})
          (each [i letter (ipairs schema)]
            (let [cost (or (. costs i) 0)]
              (if (= letter trunk)
                  (do
                    (if (= target trunk)
                        (when (<= trunk-lvl lvl)
                          (set total (+ total cost)))
                        (set total (+ total cost)))
                    (set trunk-lvl (+ trunk-lvl 1)))
                  (do
                    (when (not (. branch-lvls letter))
                      (set (. branch-lvls letter) trunk-lvl))
                    (when (and (= target letter)
                               (<= (. branch-lvls letter) lvl))
                      (set total (+ total cost)))
                    (set (. branch-lvls letter) (+ (. branch-lvls letter) 1))))))
          total))))

(fn resolve-branch [ctx br]
  (if (or (not br) (= br ""))
      (or ctx.branch "")
      (let [bmap (or ctx.branch-map (and ctx.config ctx.config.branch-map))]
        (if (and bmap (. bmap br)) (. bmap br)
            (and bmap (. bmap (br:gsub "%s+" ""))) (. bmap (br:gsub "%s+" ""))
            br))))

(fn ctx-costs [ctx]
  (or ctx.costs (and ctx.config ctx.config.costs)))

(fn ctx-schema [ctx]
  (or ctx.schema (and ctx.config ctx.config.schema)))

(fn sum-cost [ctx]
  (let [costs (ctx-costs ctx)]
    (when (not costs)
      (error "eval: $FNC-TOTALPRICE$ needs ctx.costs or ctx.config.costs"))
    (sum-series costs ctx.level (or ctx.branch "") (ctx-schema ctx))))

(fn expr-bindings [ctx ?s]
  (let [b {}]
    (each [k v (pairs (or ctx.row {}))]
      (when (= (type v) :number)
        (when (or (not ?s) (?s:find k 1 true))
          (set (. b k) v))
        (when (k:find "%s")
          (let [(stripped) (k:gsub "%s+" "")]
            (when (or (not ?s) (?s:find stripped 1 true))
              (set (. b stripped) v))))))
    ;; pin/series ctx.level overrides row Level for #expr
    (when (and (not= ctx.level nil) (or (not ?s) (?s:find "Level" 1 true)))
      (set b.Level ctx.level))
    b))

(fn ident-byte? [c]
  (and c (or (<= 48 c 57) (<= 65 c 90) (<= 97 c 122) (= c 95))))

(fn mid-multiword? [s a]
  ;; Damage tail i.e. "Critical Damage"
  ;; not the bare name after " + "
  (var i (- a 1))
  (while (and (> i 0) (= (s:byte i) 32))
    (set i (- i 1)))
  (and (> i 0) (let [c (s:byte i)]
                 (or (ident-byte? c) (= c 93)))))

;; 0 and -0 share a key but print differently, NaN is no key
(local num-strs {})
(fn num-str [n]
  (if (and (not= n 0) (= n n))
      (or (. num-strs n) (let [str (tostring n)]
                           (set (. num-strs n) str)
                           str))
      (tostring n)))

(fn materialize-ident [s name val]
  ;; skipped hits are copied along with the next replaced one
  (let [nl (length name)]
    (var a (s:find name 1 true))
    (if (not a)
        s
        (let [v (num-str val)]
          (var out nil)
          (var from 1)
          (while a
            (let [prev (s:byte (- a 1))
                  nxt (s:byte (+ a nl))]
              (when (and (not= prev 46) (not (ident-byte? prev))
                         (or (not= prev 32) (not (mid-multiword? s a)))
                         (not (ident-byte? nxt)))
                (set out (.. (or out "") (s:sub from (- a 1)) v))
                (set from (+ a nl))))
            (set a (s:find name (+ a nl) true)))
          (if out (.. out (s:sub from)) s)))))

(fn materialize-expr [source bindings]
  ;; #expr has no variables
  ;; Scribunto does not preprocess PF args
  (var s (or source ""))
  (when bindings
    (let [(k v) (next bindings)]
      (if (= (next bindings k) nil)
          (when (= (type v) :number)
            (set s (materialize-ident s k v)))
          (let [names []]
            (each [k v (pairs bindings)]
              (when (and (= (type v) :number) (s:find k 1 true))
                (table.insert names k)))
            (table.sort names longer-first?)
            (each [_ name (ipairs names)]
              (set s (materialize-ident s name (. bindings name))))))))
  s)

(fn remote-cell-number [ctx row v]
  ;; cache cells may still be $CDMG$ etc.
  (if (= (type v) :number)
      v
      (let [fname (cell-formula-name v)]
        (if fname
            ;; nil on fail
            ;; #expr then errors loud on the raw name
            (let [saved ctx.row]
              (set ctx.row row)
              (let [(ok res) (pcall eval-named-formula ctx fname)]
                (set ctx.row saved)
                (and ok (= (type res) :number) res)))
            (extract-number v)))))

(fn materialize-dotrefs [source ctx]
  ;; only tables named in the body
  ;; as page cache has every table
  (var s (tostring (or source "")))
  (let [tc ctx.table-cache
        keys []
        vals {}]
    (when tc
      (each [tname levels (pairs tc)]
        (when (and (= (type tname) :string) (= (type levels) :table)
                   (not (tname:find "|" 1 true)) (s:find tname 1 true)
                   (s:find (.. tname ".") 1 true))
          (let [row (cache-get levels ctx.level ctx.branch)]
            (when row
              (each [col _ (pairs row)]
                (when (and (= (type col) :string) (s:find col 1 true)
                           (not (col:match "_ROF$")))
                  (let [key (.. tname "." col)]
                    (when (and (= (. vals key) nil) (s:find key 1 true))
                      (let [n (remote-cell-number ctx row
                                                  (row-field row col ctx.rof?))]
                        ;; false = miss, not looked up again
                        (set (. vals key) (or n false))
                        (when n
                          (table.insert keys key))))))))))))
    (table.sort keys longer-first?)
    (each [_ key (ipairs keys)]
      (let [vs (tostring (. vals key))
            vlen (length vs)]
        (var from 1)
        (var done false)
        (while (not done)
          (let [(a b) (s:find key from true)]
            (if a
                (do
                  (set s (.. (s:sub 1 (- a 1)) vs (s:sub (+ b 1))))
                  (set from (+ a vlen)))
                (set done true)))))))
  s)

;; #expr is pure, $FNC-TOTAL-X$ repeats it per level per row
;; number kept too, tonumber per hit is slow
(fn parser-expr [frame expr]
  (let [memo (or (. expr-memos frame)
                 (let [m {}]
                   (set (. expr-memos frame) m)
                   m))
        hit (. memo expr)]
    (if (not= hit nil)
        hit
        (let [out (tostring (or (frame:callParserFunction {:name "#expr"
                                                           :args [expr]})
                                ""))
              res [out (tonumber out)]]
          (set (. memo expr) res)
          res))))

(fn call-parser-expr [frame expr]
  (. (parser-expr frame expr) 2))

(fn alpha-byte? [c]
  (and c (or (<= 65 c 90) (<= 97 c 122))))

(fn has-table-col? [expr]
  ;; lua find "." is too eager
  ;; %a%.%a tries every position
  (var a (expr:find "." 1 true))
  (var found false)
  (while (and a (not found))
    (set found (and (alpha-byte? (expr:byte (- a 1)))
                    (alpha-byte? (expr:byte (+ a 1)))))
    (set a (expr:find "." (+ a 1) true)))
  found)

(fn eval-dollar-in-expr [name ctx]
  (let [env (or ctx.formula-env (and ctx.config ctx.config.formula-env) {})
        cache (or ctx.parse-cache {})
        stack (or ctx.ident-stack {})]
    (when (. stack name)
      (error (.. "eval: cyclic formula '" name "'")))
    (set (. stack name) true)
    (set ctx.ident-stack stack)
    (let [ast (parser.parse-var name env cache [])
          (ok res) (pcall eval-node ctx ast)]
      (set (. stack name) nil)
      (if (not ok) (error res 0)
          (not= (type res) :number) (error (.. "eval: $" name
                                               "$ in #expr is not a number"))
          res))))

(fn materialize-dollars [source ctx]
  (if (not (source:find "$" 1 true))
      source
      (pick-values 1
                   (source:gsub "%$([^%$]+)%$"
                                (fn [name]
                                  (tostring (eval-dollar-in-expr name ctx)))))))

(fn materialize-expr-body [source ctx]
  ;; Table.Col first as ltr expand may already put things on the row
  ;; i.e. Damage=n
  (let [src (materialize-dollars (tostring (or source "")) ctx)
        expr0 (if (has-table-col? src)
                  (materialize-dotrefs src ctx)
                  src)
        b (expr-bindings ctx expr0)]
    (materialize-expr expr0 b)))

(fn expand-inline-expr [text ctx]
  (pick-values 1
               (: (tostring (or text "")) :gsub "{{#expr:(.-)}}"
                  (fn [body]
                    (let [(ok expr) (pcall materialize-expr-body body ctx)
                          expr (if ok expr body)
                          frame ctx.frame]
                      (if (and frame frame.callParserFunction)
                          (. (parser-expr frame expr) 1)
                          (tostring (or (mwexpr.eval expr {}) ""))))))))

(fn eval-mw-expr-node [source ctx]
  (let [expr (materialize-expr-body source ctx)
        frame ctx.frame]
    (if (and frame frame.callParserFunction)
        (let [n (call-parser-expr frame expr)]
          (when (= n nil)
            (error (.. "eval: #expr returned non-number for: " expr)))
          n)
        (mwexpr.eval expr {}))))

(fn substitute-row-keys [text row level]
  (var s (tostring (or text "")))
  (let [keys []]
    (each [k v (pairs (or row {}))]
      (when (= (type v) :number)
        (table.insert keys k)))
    (table.sort keys longer-first?)
    (when (and level (= (. row "Level") nil))
      (set s (pick-values 1 (s:gsub "Level" (tostring level)))))
    (each [_ k (ipairs keys)]
      (when (s:find k 1 true)
        (let [pat (k:gsub "([^%w])" "%%%1")]
          (set s (pick-values 1 (s:gsub pat (tostring (. row k))))))))
    (when (and level (s:find "Level" 1 true))
      (set s
           (pick-values 1
                        (s:gsub "Level" (tostring (or (. row "Level") level))))))
    s))

(fn eval-wikitext [raw ctx]
  ;; whole-string tonumber only
  ;; there shooould be no 1st digit scrape
  ;; #expr first so local Damage cannot eat Table.Damage
  (let [expanded (expand-inline-expr raw ctx)
        subbed (substitute-row-keys expanded (or ctx.row {}) ctx.level)
        frame ctx.frame
        done (if (and frame frame.preprocess)
                 (frame:preprocess subbed)
                 subbed)
        s (tostring done)
        trimmed (pick-values 1 (s:match "^%s*(.-)%s*$"))
        n (tonumber trimmed)]
    (if (not= n nil) n done)))

(fn resolve-named-ast [ctx name]
  (let [asts (or ctx.formula-asts ctx.parse-cache)
        env (or ctx.formula-env (and ctx.config ctx.config.formula-env))
        vars (or ctx.vars (and ctx.config ctx.config.vars) {})
        prefix (or ctx.prefix (and ctx.config ctx.config.prefix) "")
        key (config.get-array-var-key vars prefix name)
        raw (or (and env (. env name)) (. vars key) "")]
    (var ast (and asts (. asts name)))
    (when (not ast)
      (when (and raw (not= raw ""))
        (let [cache (or ctx.parse-cache {})
              e (or env {name raw})]
          (set ast (parser.parse-var name e cache []))
          (when ctx.parse-cache
            (set (. ctx.parse-cache name) ast))
          (when ctx.formula-asts
            (set (. ctx.formula-asts name) ast)))))
    ast))

(fn eval-at-level [ctx lvl ast]
  (let [saved-level ctx.level
        row (or ctx.row {})
        saved-rl row.Level]
    (set ctx.level lvl)
    (set row.Level lvl)
    (let [(ok res) (pcall eval-node ctx ast)]
      (set ctx.level saved-level)
      (set row.Level saved-rl)
      (if ok res (error res 0)))))

(fn with-pin! [ctx lvl br f a b c]
  (let [saved-level ctx.level
        saved-branch ctx.branch]
    (set ctx.level (or lvl ctx.level))
    (set ctx.branch (resolve-branch ctx br))
    (let [(ok res) (pcall f a b c)]
      (set ctx.level saved-level)
      (set ctx.branch saved-branch)
      (if ok res (error res 0)))))

(fn sum-formula-series [ctx name]
  (let [ast (resolve-named-ast ctx name)]
    (when (not ast)
      (error (.. "eval: $FNC-TOTAL-" name
                 "$ needs a formula or numeric series for $" name "$")))
    (let [lvl (or (tonumber ctx.level) 0)]
      (var total 0)
      (for [i 0 lvl]
        (let [n (eval-at-level ctx i ast)]
          (when (not= (type n) :number)
            (error (.. "eval: $FNC-TOTAL-" name "$ at level " (tostring i)
                       " is not a number: " (tostring n))))
          (set total (+ total n))))
      total)))

(fn sum-total [ctx name]
  (let [series (and ctx.totals (. ctx.totals name))]
    (if series
        (sum-series series ctx.level (or ctx.branch "") (ctx-schema ctx))
        (let [vars (or ctx.vars (and ctx.config ctx.config.vars) {})
              prefix (or ctx.prefix (and ctx.config ctx.config.prefix) "")
              key (config.get-array-var-key vars prefix name)
              raw (or (. vars key) "")
              body (pick-values 1 (: (tostring raw) :match "^%s*(.-)%s*$"))]
          (if (config.is-numeric-array-body raw)
              (sum-series (config.parse-number-list raw) ctx.level
                          (or ctx.branch "") (ctx-schema ctx))
              (= body "")
              (error (.. "eval: $FNC-TOTAL-" name
                         "$ has no numeric series or formula at " (tostring key)))
              (sum-formula-series ctx name))))))

(fn binop [op lv rv]
  (case op
    "+" (+ lv rv)
    "-" (- lv rv)
    "*" (* lv rv)
    "/" (if (= rv 0)
            (error "eval: division by zero")
            (/ lv rv))
    "%" (if (= rv 0)
            (error "eval: modulo by zero")
            (% lv rv))
    _ (error (.. "eval: unknown binop " (tostring op)))))

;; dispatch table are cheaper than Fennel's `match` on every node
(local node-handlers {})

(fn node-handlers.num [_ctx node]
  (. node 2))

(fn node-handlers.ident [ctx node]
  (or (row-lookup ctx (. node 2)) (unresolved ctx (. node 2))))

(fn node-handlers.soft-ident [ctx node]
  ;; lone ident use column first
  ;; else display text ($Hello$ = World)
  (or (row-lookup ctx (. node 2)) (. node 2)))

(fn node-handlers.dotref [ctx node]
  (let [v (table-lookup ctx (. node 2) (. node 3))]
    (if (= v nil)
        (error (.. "eval: unresolved table lookup '" (. node 2) "." (. node 3)
                   "'"))
        v)))

(fn node-handlers.binop [ctx node]
  (binop (. node 2) (eval-node ctx (. node 3)) (eval-node ctx (. node 4))))

(fn node-handlers.pow [ctx node]
  (^ (eval-node ctx (. node 2)) (eval-node ctx (. node 3))))

(fn node-handlers.unop [ctx node]
  (- (eval-node ctx (. node 3))))

(fn node-handlers.pin [ctx node]
  (with-pin! ctx (. node 2) (. node 3) eval-node ctx (. node 4)))

(fn node-handlers.intrinsic [ctx node]
  (case (. node 2)
    :totalprice (sum-cost ctx)
    :total (sum-total ctx (. node 3))
    _ (error (.. "eval: unknown intrinsic " (tostring (. node 2))))))

(fn node-handlers.array [ctx node]
  (let [raw (. node 2)
        parts (config.split-semi raw)
        idx (array-index ctx.level (or ctx.branch "") (ctx-schema ctx))]
    (when (or (not idx) (< idx 1) (> idx (length parts)))
      (error (.. "eval: array index out of range at level "
                 (tostring (or ctx.level 0)))))
    (let [el (or (. parts idx) "")
          n (tonumber (pick-values 1 (el:gsub "," "")))]
      (if (not= n nil) n el))))

(fn node-handlers.literal [_ctx node]
  (. node 2))

(fn node-handlers.mw-expr [ctx node]
  (eval-mw-expr-node (. node 2) ctx))

(fn node-handlers.wikitext [ctx node]
  (eval-wikitext (. node 2) ctx))

(fn node-handlers.varref [_ctx node]
  (error (.. "eval: unspliced $VAR$ left in tree: $" (. node 2)
             "$ (config keys are not formula-evaluable)")))

(set eval-node
     (fn [ctx node]
       (let [tag (and (= (type node) :table) (. node 1))
             h (and tag (. node-handlers tag))]
         (if h
             (h ctx node)
             (error (.. "eval: unhandled node tag " (tostring tag)))))))

(fn eval-string [s ctx var-env]
  (let [ast (if (and var-env (next var-env))
                (let [(a _) (parser.parse-with-env s var-env)] a)
                (parser.parse-string s))]
    (eval-node ctx ast)))

(fn make-ctx [opts]
  (let [cfg opts.config
        ctx {:row (or opts.row {})
             :level (or opts.level 0)
             :branch (or opts.branch "")
             :rof? (or opts.rof? false)
             :table-cache opts.table-cache
             :frame opts.frame
             :formula-name opts.formula-name
             :formula-asts opts.formula-asts
             :parse-cache (or opts.parse-cache {})
             :config cfg
             :vars (or opts.vars (and cfg cfg.vars))
             :prefix (or opts.prefix (and cfg cfg.prefix) "")
             :costs (or opts.costs (and cfg cfg.costs))
             :schema (or opts.schema (and cfg cfg.schema))
             :branch-map (or opts.branch-map (and cfg cfg.branch-map))
             :totals opts.totals
             :formula-env (or opts.formula-env (and cfg cfg.formula-env))}]
    ctx))

;; rebind per cell
(fn bind-ctx! [ctx row rof? lvl]
  (set ctx.row (or row {}))
  (set ctx.level (or lvl 0))
  (set ctx.rof? (or rof? false))
  ctx)

{:eval-node eval-node
 :eval-string eval-string
 :make-ctx make-ctx
 :bind-ctx! bind-ctx!
 :row-lookup row-lookup
 :table-lookup table-lookup
 :cache-get cache-get
 :array-index array-index
 :sum-series sum-series
 :sum-cost sum-cost
 :sum-total sum-total
 :sum-formula-series sum-formula-series
 :resolve-branch resolve-branch
 :expr-bindings expr-bindings
 :materialize-expr materialize-expr
 :expand-inline-expr expand-inline-expr
 :call-parser-expr call-parser-expr
 :substitute-row-keys substitute-row-keys}
