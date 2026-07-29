;; The lovely AST evaluator

(local config (require :config))
(local mwexpr (require :mwexpr))

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
			(error (.. "eval: not a number for '" (or label "?") "': "
								 (tostring v))))
		n))

(fn unresolved [ctx name]
	;; missing keys hard error (NS0)
	(error (if (and ctx ctx.formula-name)
						 (.. "unresolved identifier '" name "' in formula for $"
								 ctx.formula-name "$")
						 (.. "unresolved identifier '" name "'"))))

(var eval-node nil)

(fn formula-fallback [ctx name]
	;; bare DPS2 in CE2 = Total Price / DPS2 when no DPS2 column
	(let [env (or ctx.formula-env (and ctx.config ctx.config.formula-env))
				raw (and env (. env name))]
		(when (and raw (not= raw ""))
			(let [stack (or ctx.ident-stack {})]
				(when (. stack name)
					(error (.. "eval: cyclic bare identifier '" name "'")))
				(tset stack name true)
				(set ctx.ident-stack stack)
				(let [asts (or ctx.formula-asts ctx.parse-cache)
							parser (require :parser)
							ast (or (and asts (. asts name))
											(parser.parse-var name env (or ctx.parse-cache {}) []))
							(ok res) (pcall eval-node ctx ast)]
					(tset stack name nil)
					(if ok res (error res)))))))

(fn cell-formula-name [v]
	;; $NAME$ still sitting in a cached cell (maybe glued $DPS$$RREF$)
	(when (= (type v) :string)
		(let [s (pick-values 1 (v:gsub "<ref[^>]*>.-</ref>" ""))
					s (pick-values 1 (s:gsub "<ref[^>]*/>" ""))]
			(s:match "%$([A-Za-z][%w%-]*)%$"))))

(fn with-row! [ctx row f]
	(let [saved ctx.row]
		(set ctx.row row)
		(let [(ok res) (pcall f)]
			(set ctx.row saved)
			(if ok res (error res)))))

(fn resolve-cell-value [ctx row v label]
	(if (= v nil)
			nil
			(= (type v) :number)
			v
			(let [n (extract-number v)]
				(if (not= n nil)
						n
						(let [fname (cell-formula-name v)]
							(if fname
									(with-row! ctx row #(formula-fallback ctx fname))
									(as-number v label)))))))

(fn row-lookup [ctx name]
	;; exact key then stripped dual-write
	;; missing / still-$token$ may be a formula (DPS2, remote $SDPS$)
	;; Level -> ctx.level
	(let [row (or ctx.row {})
				direct (. row name)
				v (if (not= direct nil)
							direct
							(let [stripped (pick-values 1 (name:gsub "%s+" ""))]
								(. row stripped)))]
		(if (not= v nil)
				(or (resolve-cell-value ctx row v name)
						(unresolved ctx name))
				(= name "Level")
				(if (not= ctx.level nil)
						ctx.level
						(unresolved ctx name))
				(let [fb (formula-fallback ctx name)]
					(if (not= fb nil)
							fb
							(unresolved ctx name))))))

(fn cache-get [cache level branch]
	(when cache
		(if (and branch (not= branch ""))
				(or (. cache (.. (tostring level) branch))
						(. cache level))
				(. cache level))))

(fn table-lookup [ctx tname col]
	(let [tc ctx.table-cache
				entry (and tc (or (. tc tname)
													(. tc (tname:gsub "%s+" ""))))
				row (cache-get entry ctx.level ctx.branch)]
		(if (not row)
				nil
				(let [v (if (and ctx.rof? (not= (. row (.. col "_ROF")) nil))
										(. row (.. col "_ROF"))
										(or (. row col)
												(. row (col:gsub "%s+" ""))))]
					(if (= v nil)
							nil
							(resolve-cell-value ctx row v (.. tname "." col)))))))

(fn sum-series [costs level branch schema]
	(let [lvl (or (tonumber level) 0)]
		(if (not schema)
				(accumulate [total 0 i c (ipairs (or costs []))]
					(if (<= (- i 1) lvl)
							(+ total (or c 0))
							total))
				(let [trunk (or (. schema 1) "N")
							target (if (and branch (not= branch "")) branch trunk)]
					(var total 0)
					(var trunk-lvl 0)
					(var branch-lvls {})
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
											(tset branch-lvls letter trunk-lvl))
										(when (and (= target letter)
															 (<= (. branch-lvls letter) lvl))
											(set total (+ total cost)))
										(tset branch-lvls letter
													(+ (. branch-lvls letter) 1))))))
					total))))

(fn resolve-branch [ctx br]
	(if (or (not br) (= br ""))
			(or ctx.branch "")
			(let [bmap (or ctx.branch-map
										 (and ctx.config ctx.config.branch-map))]
				(if (and bmap (. bmap br))
						(. bmap br)
						(and bmap (. bmap (br:gsub "%s+" "")))
						(. bmap (br:gsub "%s+" ""))
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

(fn expr-bindings [ctx]
	(let [b {}]
		(each [k v (pairs (or ctx.row {}))]
			(when (= (type v) :number)
				(tset b k v)
				(let [(stripped) (k:gsub "%s+" "")]
					(tset b stripped v))))
		;; pin/series ctx.level overrides row Level for #expr
		(when (not= ctx.level nil)
			(tset b "Level" ctx.level))
		b))

(fn mid-multiword? [s a]
	;; Damage tail ie "Critical Damage"
	;; not the bare name after " + "
	(var i (- a 1))
	(while (and (> i 0) (= (s:sub i i) " "))
		(set i (- i 1)))
	(and (> i 0)
			 (let [ch (s:sub i i)]
				 (or (ch:match "[%w_]") (= ch "]")))))

(fn materialize-ident [s name val]
	(var out "")
	(var from 1)
	(var done false)
	(let [v (tostring val)]
		(while (not done)
			(let [(a b) (s:find name from true)]
				(if (not a)
						(do
							(set out (.. out (s:sub from)))
							(set done true))
						(let [prev (if (= a 1) "" (s:sub (- a 1) (- a 1)))
									nxt (s:sub (+ b 1) (+ b 1))
									ok (and (not= prev ".")
													(or (= a 1) (not (prev:match "[%w_]")))
													(or (not= prev " ") (not (mid-multiword? s a)))
													(or (= nxt "") (not (nxt:match "[%w_]"))))]
							(if ok
									(do
										(set out (.. out (s:sub from (- a 1)) v))
										(set from (+ b 1)))
									(do
										(set out (.. out (s:sub from b)))
										(set from (+ b 1))))))))
		out))

(fn materialize-expr [source bindings]
	;; #expr has no variables
	;; Scribunto does not preprocess PF args
	(var s (tostring (or source "")))
	(when (and bindings (next bindings))
		(let [names []]
			(each [k v (pairs bindings)]
				(when (and (= (type v) :number) (s:find k 1 true))
					(table.insert names k)))
			(when (> (length names) 0)
				(table.sort names (fn [a b] (> (length a) (length b))))
				(each [_ name (ipairs names)]
					(set s (materialize-ident s name (. bindings name)))))))
	s)

(fn remote-cell-number [ctx row v]
	;; cache cells may still be $CDMG$ etc.
	(if (= (type v) :number)
			v
			(let [fname (cell-formula-name v)]
				(if fname
						(let [saved ctx.row]
							(set ctx.row row)
							(let [(ok res) (pcall formula-fallback ctx fname)]
								(set ctx.row saved)
								;; nil on fail
								;; #expr then errors loud on the raw name
								(and ok (= (type res) :number) res)))
						(extract-number v)))))

(fn materialize-dotrefs [source ctx]
	;; only tables named in the body
	;; as page cache has every table
	(var s (tostring (or source "")))
	(let [tc ctx.table-cache
				entries []
				seen {}]
		(when tc
			(each [tname levels (pairs tc)]
				(when (and (= (type tname) :string) (= (type levels) :table)
									 (not (tname:find "|" 1 true))
									 (s:find tname 1 true))
					(let [row (cache-get levels ctx.level ctx.branch)]
						(when row
							(each [col v (pairs row)]
								(when (and (= (type col) :string) (not (col:match "_ROF$")))
									(let [key (.. tname "." col)]
										(when (and (not (. seen key)) (s:find key 1 true))
											(tset seen key true)
											(let [n (remote-cell-number ctx row v)]
												(when n
													(table.insert entries [key n]))))))))))))
		(when (> (length entries) 0)
			(table.sort entries (fn [a b] (> (length (. a 1)) (length (. b 1)))))
			(each [_ pair (ipairs entries)]
				(let [key (. pair 1)
							vs (tostring (. pair 2))
							vlen (length vs)]
					(var from 1)
					(var done false)
					(while (not done)
						(let [(a b) (s:find key from true)]
							(if a
									(do
										(set s (.. (s:sub 1 (- a 1)) vs (s:sub (+ b 1))))
										(set from (+ a vlen)))
									(set done true))))))))
	s)

(fn call-parser-expr [frame expr]
	(let [out (frame:callParserFunction {:name "#expr" :args [expr]})]
		(tonumber (tostring out))))

(fn has-table-col? [expr]
	;; lua find "." is too eager
	(not= (expr:find "%a%.%a") nil))

(fn eval-mw-expr-node [source ctx]
	;; Table.Col first as ltr expand may already put things on the row
	;; ie Damage=n
	(let [src (tostring (or source ""))
				expr0 (if (has-table-col? src)
									(materialize-dotrefs src ctx)
									src)
				expr (materialize-expr expr0 (expr-bindings ctx))
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
		(each [k _ (pairs (or row {}))]
			(table.insert keys k))
		(table.sort keys (fn [a b] (> (length a) (length b))))
		(when (and level (= (. row "Level") nil))
			(set s (pick-values 1 (s:gsub "Level" (tostring level)))))
		(each [_ k (ipairs keys)]
			(let [v (. row k)]
				(when (= (type v) :number)
					(let [pat (k:gsub "([^%w])" "%%%1")]
						(set s (pick-values 1 (s:gsub pat (tostring v))))))))
		(when (and level (s:find "Level" 1 true))
			(set s (pick-values 1
							 (s:gsub "Level" (tostring (or (. row "Level") level))))))
		s))

(fn eval-wikitext [raw ctx]
	;; whole-string tonumber only
	;; there shooould be no 1st digit scrape
	(let [subbed (substitute-row-keys raw (or ctx.row {}) ctx.level)
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
				parser (require :parser)
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
						(tset ctx.parse-cache name ast))
					(when ctx.formula-asts
						(tset ctx.formula-asts name ast)))))
		ast))

(fn with-level! [ctx lvl f]
	(let [saved-level ctx.level
				row (or ctx.row {})
				saved-rl (. row "Level")]
		(set ctx.level lvl)
		(tset row "Level" lvl)
		(let [(ok res) (pcall f)]
			(set ctx.level saved-level)
			(tset row "Level" saved-rl)
			(if ok res (error res)))))

(fn with-pin! [ctx lvl br f]
	(let [saved-level ctx.level
				saved-branch ctx.branch]
		(set ctx.level (or lvl ctx.level))
		(set ctx.branch (resolve-branch ctx br))
		(let [(ok res) (pcall f)]
			(set ctx.level saved-level)
			(set ctx.branch saved-branch)
			(if ok res (error res)))))

(fn sum-formula-series [ctx name]
	(let [ast (resolve-named-ast ctx name)]
		(when (not ast)
			(error (.. "eval: $FNC-TOTAL-" name
								 "$ needs a formula or numeric series for $" name "$")))
		(let [lvl (or (tonumber ctx.level) 0)]
			(var total 0)
			(for [i 0 lvl]
				(let [n (with-level! ctx i #(eval-node ctx ast))]
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
							(sum-series (config.parse-number-list raw)
													ctx.level (or ctx.branch "") (ctx-schema ctx))
							(= body "")
							(error (.. "eval: $FNC-TOTAL-" name
												 "$ has no numeric series or formula at "
												 (tostring key)))
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
	(row-lookup ctx (. node 2)))

(fn node-handlers.dotref [ctx node]
	(let [v (table-lookup ctx (. node 2) (. node 3))]
		(if (= v nil)
				(error (.. "eval: unresolved table lookup '" (. node 2) "."
									 (. node 3) "'"))
				v)))

(fn node-handlers.binop [ctx node]
	(binop (. node 2)
				 (eval-node ctx (. node 3))
				 (eval-node ctx (. node 4))))

(fn node-handlers.pow [ctx node]
	(^ (eval-node ctx (. node 2)) (eval-node ctx (. node 3))))

(fn node-handlers.unop [ctx node]
	(- (eval-node ctx (. node 3))))

(fn node-handlers.pin [ctx node]
	(with-pin! ctx (. node 2) (. node 3)
						 #(eval-node ctx (. node 4))))

(fn node-handlers.intrinsic [ctx node]
	(case (. node 2)
		:totalprice (sum-cost ctx)
		:total (sum-total ctx (. node 3))
		_ (error (.. "eval: unknown intrinsic " (tostring (. node 2))))))

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
	(let [parser (require :parser)
				ast (if (and var-env (next var-env))
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

{:eval-node eval-node
 :eval-string eval-string
 :make-ctx make-ctx
 :row-lookup row-lookup
 :table-lookup table-lookup
 :cache-get cache-get
 :sum-series sum-series
 :sum-cost sum-cost
 :sum-total sum-total
 :sum-formula-series sum-formula-series
 :resolve-branch resolve-branch
 :expr-bindings expr-bindings
 :materialize-expr materialize-expr
 :call-parser-expr call-parser-expr
 :substitute-row-keys substitute-row-keys}
