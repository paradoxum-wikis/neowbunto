;; Page render

(local config (require :config))
(local parser (require :parser))
(local eval (require :eval))
(local wikitable (require :wikitable))
(local tablecache (require :tablecache))

(local byte string.byte)

(local ws-byte {32 true 9 true 10 true 11 true 12 true 13 true})

(fn trim [s]
  (let [str (tostring (or s ""))]
    (if (or (. ws-byte (byte str 1)) (. ws-byte (byte str -1)))
        (let [a (or (str:find "%S") (+ (length str) 1))]
          (var b (length str))
          (while (and (>= b a) (str:find "^%s" b))
            (set b (- b 1)))
          (str:sub a b))
        str)))

(fn each-line [text]
  (var pos 1)
  (fn []
    (when pos
      (let [nl (text:find "\n" pos true)
            line (text:sub pos (- (or nl (+ (length text) 1)) 1))]
        (set pos (and nl (+ nl 1)))
        line))))

;; formatNum is a PHP bridge
;; warm once per page
(var cached-format-num false)
(var format-memo {})
(local format-strs {2 "%.2f" 3 "%.3f"})

(fn warm-format-num! []
  (when (= cached-format-num false)
    (let [lang (and mw mw.language (mw.language.getContentLanguage))]
      (set cached-format-num (if (and lang lang.formatNum)
                                 (fn [n] (lang:formatNum n))
                                 nil))))
  cached-format-num)

(fn strip-zeros [raw]
  (if (= (byte raw -1) 48)
      (pick-values 1 (raw:gsub "%.?0+$" ""))
      raw))

(fn fmt [n dec]
  (if (not= (type n) :number)
      (tostring n)
      (do
        (when (= cached-format-num false)
          (warm-format-num!))
        (let [d (or dec 2)
              raw (: (or (. format-strs d) (.. "%." d "f")) :format n)]
          (if cached-format-num
              (or (. format-memo raw)
                  (let [out (cached-format-num (tonumber (strip-zeros raw)))]
                    (set (. format-memo raw) out)
                    out))
              (tostring (tonumber (strip-zeros raw))))))))

(fn extract-prefix [pre-text]
  (var last "")
  (local text (tostring (or pre-text "")))
  (each [name (text:gmatch "|%s*([^|\n]+)%s*")]
    (let [raw (trim name)]
      (when (and (not (raw:match "^%-*$")) (not (raw:match "=%s*$")))
        (let [(n) (raw:gsub "[^%a%d]+" "")]
          (when (and (not= n "") (not= (n:lower) "tabber"))
            (set last n))))))
  (when (and (= last "") (text:find "-" 1 true) (text:find "=" 1 true))
    (each [name (text:gmatch "%-?%-%s*|?%s*([^|\n<>={}\t/]+)%s*=%s*")]
      (let [(n) (: (trim name) :gsub "[^%a%d]+" "")]
        (when (not= n "")
          (set last n)))))
  last)

(fn new-toggle-builder [?seed]
  (when ?seed
    (math.randomseed ?seed))
  (when (and (not ?seed) os os.clock)
    (math.randomseed (math.floor (* (os.clock) 777777777))))
  {:id (.. "rofbug-" (tostring (math.random 100000 999999)))
   :classes []
   :cell-count 0})

(fn next-cell-toggle! [builder]
  (let [cell-id (.. builder.id "-c" builder.cell-count)]
    (set builder.cell-count (+ builder.cell-count 1))
    (table.insert builder.classes (.. "mw-customtoggle-" cell-id "-off"))
    (table.insert builder.classes (.. "mw-customtoggle-" cell-id "-on"))
    cell-id))

(fn rof-cell-html [cell-id v-norm v-rof]
  (.. "<span class=\"mw-collapsible mw-collapsed\" id=\"mw-customcollapsible-"
      cell-id "-off\">" v-norm "</span>"
      "<span class=\"mw-collapsible\" id=\"mw-customcollapsible-" cell-id
      "-on\">" v-rof "</span>"))

(fn history-page? []
  ;; never show .open-se on /History
  ;; not until I add historical data support for SE, at least
  (let [title (and mw mw.title (mw.title.getCurrentTitle))
        page-name (or (and title title.text) "")]
    (and (not= page-name "") (page-name:find "/History" 1 true) true)))

(fn make-editor-btn-html [is-fse]
  (if (or (not is-fse) (history-page?))
      nil
      (let [title (and mw mw.title (mw.title.getCurrentTitle))
            page-name (or (and title title.text) "TestPage")
            enc (if (and mw mw.uri mw.uri.encode)
                    (mw.uri.encode page-name "PATH")
                    page-name)]
        ;; see [[MediaWiki:Shared.css]] for class
        (.. "<div class=\"wds-button open-se\">" "[https://se.tds.wiki/tower/"
            enc " Open in Statistics Editor]" "</div>"))))

(fn wrap-rofbug [table-out builder is-fse]
  (let [label-id (.. builder.id "-label")
        classes []]
    (each [_ c (ipairs builder.classes)]
      (table.insert classes c))
    (table.insert classes (.. "mw-customtoggle-" label-id "-off"))
    (table.insert classes (.. "mw-customtoggle-" label-id "-on"))
    (let [btn-classes (table.concat classes " ")
          btn (.. "<div class=\"wds-button " btn-classes "\">"
                  "<span class=\"mw-collapsible\" id=\"mw-customcollapsible-"
                  label-id "-on\">Disable Rate of Fire Bug</span>"
                  "<span class=\"mw-collapsible mw-collapsed\" id=\"mw-customcollapsible-"
                  label-id "-off\">Enable Rate of Fire Bug</span>" "</div>")
          editor (or (make-editor-btn-html is-fse) "")
          body (.. "<div>\n" table-out "\n</div>")]
      (.. "<div class=\"rofbug-wrapper\">" btn editor body "</div>"))))

(fn formula-name-evaluable? [name formula-env]
  ;; pins (@N) are relatively rare
  (if (. formula-env name)
      true
      (= name "FNC-TOTALPRICE")
      true
      (name:match "^FNC%-TOTAL%-(.+)$")
      true
      (name:find "@" 1 true)
      (let [(base _ _) (parser.parse-pin-parts name)]
        (or (. formula-env base) (= base "FNC-TOTALPRICE")
            (not= (base:match "^FNC%-TOTAL%-(.+)$") nil)))
      false))

;; free fn pcall(f, ...) so we do not alloc a closure per $VAR$
(fn eval-known-var [name ctx formula-env parse-cache]
  (eval.eval-node ctx (parser.parse-var name formula-env parse-cache [])))

(fn eval-var-name [name ctx formula-env parse-cache]
  ;; unknown -> nil (leave token)
  ;; known still pcall so one bad cell
  ;; (N/A, wikilinks) does not abort the whole page
  (let [ast (and (. formula-env name) (. parse-cache name))]
    (if ast
        (let [(ok result) (pcall eval.eval-node ctx ast)]
          (if ok result nil))
        (if (not (formula-name-evaluable? name formula-env))
            nil
            (let [(ok result) (pcall eval-known-var name ctx formula-env
                                     parse-cache)]
              (if ok result nil))))))

;; module-level so gsub gets one shared callback
;; only safe while eval never calls back into render
(var xd-ctx nil)
(var xd-env nil)
(var xd-cache nil)
(var xd-depth 0)
(var xd-expand nil)

(fn expand-token [name]
  (let [r (eval-var-name name xd-ctx xd-env xd-cache)]
    (if (= r nil)
        (.. "$" name "$")
        (= (type r) :number)
        (fmt r)
        (let [str (tostring r)]
          ;; "$", not pattern "%$"
          (if (str:find "$" 1 true)
              (xd-expand str)
              str)))))

(set xd-expand (fn [s]
                 (set xd-depth (+ xd-depth 1))
                 (when (> xd-depth 12)
                   (error "expand-dollars: nesting too deep"))
                 (let [out (pick-values 1
                                        (: (tostring (or s "")) :gsub
                                           "%$([^%$]+)%$" expand-token))]
                   (set xd-depth (- xd-depth 1))
                   out)))

(fn expand-dollars [text ctx formula-env parse-cache]
  ;; recurse into string results so gsub does not rescan replacements
  (set xd-ctx ctx)
  (set xd-env formula-env)
  (set xd-cache parse-cache)
  (set xd-depth -1)
  (xd-expand text))

(fn coerce-row-value [v]
  (or (tonumber v)
      (let [m (and (= (type v) :string) (v:match "[%d,]+%.?%d*"))]
        (or (and m (tonumber (pick-values 1 (m:gsub "," "")))) v))))

(fn detect-table-branch [tbl branch-map]
  (var branch "")
  (var done false)
  (when (: (tbl:lower) :find "colspan" 1 true)
    (each [line (each-line tbl) &until done]
      (let [t (trim line)]
        (when (and (= (byte t 1) 33) (: (t:lower) :find "colspan" 1 true))
          (let [name (t:match "%|(.+)$")]
            (when name
              (let [bn (trim name)]
                (set branch (or (and branch-map (. branch-map bn))
                                (and branch-map
                                     (. branch-map (bn:gsub "%s+" "")))
                                "")))))
          (set done true)))))
  branch)

(fn make-eval-base [vars
                    prefix
                    formula-env
                    parse-cache
                    frame
                    table-cache
                    branch
                    ?branch-map]
  ;; COST/SCHEMA once per table not per cell
  ;; tab-scope cache so same title i.e. Regular/PVP do not collide
  (let [prefix (or prefix "")
        costs (config.parse-number-list (or (. vars
                                               (config.get-var-key vars prefix
                                                                   "COST"))
                                            ""))
        schema (config.parse-schema (. vars
                                       (config.get-var-key vars prefix "SCHEMA")))
        bmap (or ?branch-map {})
        tc (tablecache.scope-page-cache table-cache prefix)
        cfg {:vars vars
             :prefix prefix
             :costs costs
             :schema schema
             :branch-map bmap
             :formula-env formula-env}]
    {:vars vars
     :prefix prefix
     :formula-env formula-env
     :parse-cache parse-cache
     :formula-asts parse-cache
     :frame frame
     :table-cache tc
     :branch (or branch "")
     :branch-map bmap
     :costs costs
     :schema schema
     :config cfg}))

(fn build-eval-ctx [base row rof? lvl]
  (eval.make-ctx {:row (or row {})
                  :level (or lvl 0)
                  :branch base.branch
                  :rof? (or rof? false)
                  :table-cache base.table-cache
                  :frame base.frame
                  :vars base.vars
                  :prefix base.prefix
                  :formula-env base.formula-env
                  :parse-cache base.parse-cache
                  :formula-asts base.formula-asts
                  :costs base.costs
                  :schema base.schema
                  :branch-map base.branch-map
                  :config base.config}))

(fn set-row-field! [row header stripped value]
  (set (. row header) value)
  (when (not= stripped header)
    (set (. row stripped) value))
  value)

;; reused, a fresh {} rehashes as keys go in
(fn clear-row! [row]
  (each [k _ (pairs row)]
    (set (. row k) nil))
  row)

(fn process-table [tbl
                   vars
                   formula-env
                   parse-cache
                   rof-cols
                   rof-offset
                   branch-map
                   frame
                   builder
                   table-cache
                   prefix
                   ?cached-base
                   ?cached-rc-aliases
                   ?parsed]
  (let [headers []
        header-keys []
        out []
        recursion {}
        line-cells (. (or ?parsed (wikitable.parse-table tbl {} true))
                      :line-cells)
        ;; shared with the page cache rows, never write into it
        outc []
        rof-cols (or rof-cols {})
        has-rof (next rof-cols)
        branch (detect-table-branch tbl branch-map)
        base (or ?cached-base
                 (make-eval-base vars prefix formula-env parse-cache frame
                                 table-cache branch branch-map))
        _ (set base.branch branch)
        ctx-n (build-eval-ctx base {} false 0)
        ctx-r (when has-rof (build-eval-ctx base {} true 0))
        rc-aliases (or ?cached-rc-aliases
                       (collect [k v (pairs vars)]
                         (when (= (trim v) "$FNC-RECURSION$")
                           (values k true))))
        has-rc (next rc-aliases)]
    (var level -1)
    (var col-idx 1)
    ;; lines must be classified exactly like parse-table or li drifts
    (var li 0)
    (var row-norm {})
    (var spare-norm {})
    (var bound-norm nil)
    (local row-rof (and has-rof {}))

    (fn expand-rc [s]
      (pick-values 1 (s:gsub "%$([^%$]+)%$"
                             (fn [name]
                               (if (. rc-aliases (.. "$" name "$"))
                                   "$FNC-RECURSION$"
                                   (.. "$" name "$"))))))

    (fn bind-row! []
      (set bound-norm row-norm)
      (eval.bind-ctx! ctx-n row-norm false level)
      (when has-rof
        (eval.bind-ctx! ctx-r row-rof true level)))

    (each [line (each-line tbl)]
      ;; split-row-cells trims the cells
      (let [t (if (. ws-byte (byte line 1)) (trim line) line)
            b (byte t 1)]
        (if (and (= b 124) (= (byte t 2) 45))
            (do
              (set col-idx 1)
              ;; a header line after |- still evals against the bound row,
              ;; so swap in the spare instead of clearing it
              (if (= bound-norm row-norm)
                  (let [old row-norm]
                    (set row-norm (clear-row! spare-norm))
                    (set spare-norm old))
                  (clear-row! row-norm))
              (when has-rof
                (clear-row! row-rof))
              (when (>= level 0)
                (set-row-field! row-norm "Level" "Level" level)
                (when has-rof
                  (set-row-field! row-rof "Level" "Level" level)))
              (table.insert out line))
            (and (= b 33) (not (: (t:lower) :find "colspan" 1 true)))
            (do
              (set li (+ li 1))
              (if (t:find "$" 1 true)
                  (let [body (t:sub 2)
                        (norm) (body:gsub "%|%|" "!!")]
                    (each [cell (: (.. norm "!!") :gmatch "([^!]*)!!")]
                      (let [hc (trim cell)]
                        (when (not= hc "")
                          (let [temp (expand-dollars hc ctx-n formula-env
                                                     parse-cache)
                                clean (wikitable.clean-header-text temp)]
                            (when (not= clean "")
                              (table.insert headers clean)
                              (table.insert header-keys
                                            (pick-values 1
                                                         (clean:gsub "%s+" "")))))))))
                  (each [_ h (ipairs (. line-cells li))]
                    (table.insert headers h)
                    (table.insert header-keys (pick-values 1 (h:gsub "%s+" "")))))
              (table.insert out line))
            (and (= b 124) (not= (byte t 2) 125))
            (do
              (set li (+ li 1))
              (local cells (. line-cells li))
              (bind-row!)
              (each [ci cell0 (ipairs cells)]
                (var cell cell0)
                (when has-rc
                  (let [expanded (expand-rc cell)]
                    (if (expanded:find "$FNC-RECURSION$" 1 true)
                        (let [(content0) (expanded:gsub "%$FNC%-RECURSION%$" "")
                              content (trim content0)]
                          (if (not= content "")
                              (do
                                (set (. recursion col-idx) content)
                                (set cell content))
                              (set cell (or (. recursion col-idx) ""))))
                        (set (. recursion col-idx) nil))))
                (let [h (. headers col-idx)
                      h-key (. header-keys col-idx)]
                  (when (= h "Level")
                    (let [keys (wikitable.parse-level-keys cell)]
                      (if (> (length keys) 0)
                          (let [k0 (. keys 1)
                                k0s (tostring k0)
                                from-str (k0s:match "^%d+")]
                            (set level
                                 (or (and (= (type k0) :number) k0)
                                     (and from-str (tonumber from-str))
                                     (+ level 1))))
                          (set level (or (tonumber cell) (+ level 1)))))
                    (set-row-field! row-norm "Level" "Level" level)
                    (when has-rof
                      (set-row-field! row-rof "Level" "Level" level))
                    (bind-row!))
                  (var v-norm cell)
                  (var v-rof cell)
                  ;; dual expand only with ROF as formulas may read things like Firerate_ROF
                  (when (cell:find "$" 1 true)
                    (set v-norm
                         (expand-dollars cell ctx-n formula-env parse-cache))
                    (when has-rof
                      (set v-rof
                           (expand-dollars cell ctx-r formula-env parse-cache))))
                  (when (v-norm:find "{{#expr:" 1 true)
                    (set v-norm (eval.expand-inline-expr v-norm ctx-n)))
                  (when (and has-rof (v-rof:find "{{#expr:" 1 true))
                    (set v-rof (eval.expand-inline-expr v-rof ctx-r)))
                  (when h
                    (set-row-field! row-norm h h-key (coerce-row-value v-norm))
                    (when has-rof
                      (set-row-field! row-rof h h-key (coerce-row-value v-rof))))
                  (when (and has-rof h (. rof-cols h))
                    (let [n (or (tonumber v-norm)
                                (tonumber (v-norm:match "%d+%.?%d*")))]
                      (when n
                        (let [rv (tablecache.rof-bug n rof-offset)]
                          (set-row-field! row-rof h h-key rv)
                          (set v-rof (fmt rv 3))))))
                  (if (and has-rof (not= (tostring v-norm) (tostring v-rof)))
                      (let [cell-id (next-cell-toggle! builder)]
                        (set (. outc ci)
                             (rof-cell-html cell-id (tostring v-norm)
                                            (tostring v-rof))))
                      (set (. outc ci) v-norm))
                  (set col-idx (+ col-idx 1))))
              (table.insert out
                            (.. "|" (table.concat outc "||" 1 (length cells)))))
            (table.insert out line))))
    (table.concat out "\n")))

(fn get-attr [attrs attr-name]
  (or (attrs:match (.. attr-name "%s*=%s*\"([^\"]+)\""))
      (attrs:match (.. attr-name "%s*=%s*'([^']+)'"))
      (attrs:match (.. attr-name "%s*=%s*([^%s\"'/>]+)"))))

(fn finalize-output [o vars formula-env parse-cache opts]
  ;; free-text $VAR$ (Help: "... is $EQ$!") + leftovers after table expand
  ;; same $...$ pattern as expand-dollars; named <ref> first full, later />
  (let [opts (or opts {})
        token-count {}
        ref-seen {}
        base (make-eval-base vars (or opts.prefix "") formula-env parse-cache
                             opts.frame (or opts.table-cache {}) ""
                             (or opts.branch-map {}))
        ctx (build-eval-ctx base {} false 0)]
    (if (not (o:find "$" 1 true))
        o
        (do
          (each [name (o:gmatch "%$([^%$]+)%$")]
            (let [tok (.. "$" name "$")]
              (set (. token-count tok) (+ (or (. token-count tok) 0) 1))))
          (pick-values 1
                       (o:gsub "%$([^%$]+)%$"
                               (fn [name]
                                 (let [tok (.. "$" name "$")
                                       r (eval-var-name name ctx formula-env
                                                        parse-cache)]
                                   (if (= r nil)
                                       ;; unevaluable FNC-* config keys are not display text
                                       (if (name:match "^FNC%-") "" tok)
                                       (= (type r) :number)
                                       (fmt r)
                                       (do
                                         (local str (tostring r))
                                         (let [(ref-attrs ref-content) (str:match "^%s*<ref%s*([^>]*)>(.-)</ref>%s*$")]
                                           (if (and ref-attrs
                                                    (> (or (. token-count tok)
                                                           0)
                                                       1))
                                               (let [bare (pick-values 1
                                                                       (name:gsub "[^%a%d]"
                                                                                  ""))
                                                     ref-name (or (get-attr ref-attrs
                                                                            "name")
                                                                  (bare:lower))
                                                     ref-group (get-attr ref-attrs
                                                                         "group")
                                                     group-attr (if ref-group
                                                                    (.. " group=\""
                                                                        ref-group
                                                                        "\"")
                                                                    "")
                                                     count (+ (or (. ref-seen
                                                                     tok)
                                                                  0)
                                                              1)]
                                                 (set (. ref-seen tok) count)
                                                 (if (= count 1)
                                                     (.. "<ref" group-attr
                                                         " name=\"" ref-name
                                                         "\">" ref-content
                                                         "</ref>")
                                                     (.. "<ref" group-attr
                                                         " name=\"" ref-name
                                                         "\"/>")))
                                               (if (str:find "$" 1 true)
                                                   (expand-dollars str ctx
                                                                   formula-env
                                                                   parse-cache)
                                                   str)))))))))))))

(fn build-out [content vars cfg frame builder]
  (let [formula-env (or (and cfg cfg.formula-env) (config.formula-env vars))
        parse-cache {}
        rof-cols (or (and cfg cfg.rof-cols) {})
        rof-offset (and cfg cfg.rof-offset)
        prefix0 (or (and cfg cfg.prefix) "")
        page-bmap (or (and cfg cfg.branch-map) {})
        spans (wikitable.find-table-spans content)
        parsed []
        table-prefixes []
        pres []
        base-cache {}
        rc-aliases (collect [k v (pairs vars)]
                     (when (= (trim v) "$FNC-RECURSION$")
                       (values k true)))
        result []
        n (length content)]
    ;; prefixes while parsing
    ;; register needs them before process-table
    (var cursor 1)
    (var last-prefix "")
    (each [_ span (ipairs spans)]
      (let [pre (if (< cursor span.start)
                    (content:sub cursor (- span.start 1))
                    "")
            extracted (extract-prefix pre)
            prefix (if (not= extracted "") extracted last-prefix)]
        (when (not= extracted "")
          (set last-prefix extracted))
        (table.insert pres pre)
        (table.insert parsed (wikitable.parse-table span.text page-bmap true))
        (table.insert table-prefixes prefix)
        (set cursor (+ span.stop 1))))
    (let [page-cache (tablecache.build-page-cache parsed
                                                  {:prefix prefix0
                                                   :table-prefixes table-prefixes
                                                   :index-overrides (or (and cfg
                                                                             cfg.index)
                                                                        [])
                                                   :rof-cols rof-cols
                                                   :rof-offset rof-offset})]
      (each [i span (ipairs spans)]
        (let [pre (. pres i)
              prefix (. table-prefixes i)]
          ;; free text kept raw until finalize-output (needs ref counts)
          (when (not= pre "")
            (table.insert result pre))
          (let [cached-base (. base-cache prefix)
                branch-map (if cached-base
                               cached-base.branch-map
                               (config.build-branch-map vars prefix))
                base (or cached-base
                         (make-eval-base vars prefix formula-env parse-cache
                                         frame page-cache "" branch-map))
                _ (set (. base-cache prefix) base)
                processed (process-table span.text vars formula-env parse-cache
                                         rof-cols rof-offset branch-map frame
                                         builder page-cache prefix base
                                         rc-aliases (. parsed i))]
            (table.insert result processed))))
      (when (<= cursor n)
        (table.insert result (content:sub cursor)))
      (trim (finalize-output (table.concat result) vars formula-env parse-cache
                             {:frame frame
                              :table-cache page-cache
                              :prefix prefix0
                              :branch-map page-bmap})))))

(fn blank-byte? [b]
  (or (= b 32) (= b 9)))

(fn strip-var-blocks [content]
  ;; gsub "[ \t]*<var>.-</var>[ \t]*\n?" by hand, the pattern tries every char
  (let [c (tostring (or content ""))
        out []]
    (var cursor 1)
    (var pos 1)
    (var more true)
    (while more
      (let [p (c:find "<var>" pos true)
            q (and p (c:find "</var>" (+ p 5) true))]
        (if (not q)
            (set more false)
            (do
              (var s p)
              (while (and (> s cursor) (blank-byte? (byte c (- s 1))))
                (set s (- s 1)))
              (var e (+ q 6))
              (while (blank-byte? (byte c e))
                (set e (+ e 1)))
              (when (= (byte c e) 10)
                (set e (+ e 1)))
              (table.insert out (c:sub cursor (- s 1)))
              (set cursor e)
              (set pos e)))))
    (table.insert out (c:sub cursor))
    (table.concat out)))

(fn render-page [content frame ?opts]
  (let [opts (or ?opts {})
        _ (do
            (set cached-format-num false)
            (set format-memo {})
            (warm-format-num!))
        (cfg is-fse) (config.parse-page-config content (or opts.prefix ""))
        vars cfg.vars
        body (strip-var-blocks content)
        has-rof (next cfg.rof-cols)
        builder (new-toggle-builder opts.seed)
        out (build-out body vars cfg frame builder)
        fse? (or is-fse opts.is-fse)]
    ;; FSE-only where empty wrapper shell + body sibling
    (if has-rof
        (wrap-rofbug out builder fse?)
        fse?
        (let [editor (or (make-editor-btn-html true) "")]
          (.. "<div class=\"rofbug-wrapper\">" editor "</div>\n" out))
        out)))

{:fmt fmt
 :extract-prefix extract-prefix
 :new-toggle-builder new-toggle-builder
 :next-cell-toggle! next-cell-toggle!
 :rof-cell-html rof-cell-html
 :make-editor-btn-html make-editor-btn-html
 :history-page? history-page?
 :wrap-rofbug wrap-rofbug
 :process-table process-table
 :build-out build-out
 :strip-var-blocks strip-var-blocks
 :render-page render-page
 :finalize-output finalize-output
 :expand-dollars expand-dollars}
