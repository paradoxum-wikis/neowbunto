;; Page render

(local config (require :config))
(local parser (require :parser))
(local eval (require :eval))
(local wikitable (require :wikitable))
(local tablecache (require :tablecache))

(fn trim [s]
	(let [str (tostring (or s ""))]
		(pick-values 1 (str:match "^%s*(.-)%s*$"))))

;; formatNum is a PHP bridge
;; warm once per page
(var cached-format-num false)

(fn warm-format-num! []
	(when (= cached-format-num false)
		(let [lang (and mw mw.language (mw.language.getContentLanguage))]
			(set cached-format-num
					 (if (and lang lang.formatNum)
							 (fn [n] (lang:formatNum n))
							 nil))))
	cached-format-num)

(fn fmt [n dec]
	(if (not= (type n) :number)
			(tostring n)
			(do
				(when (= cached-format-num false)
					(warm-format-num!))
				(let [d (or dec 2)
							raw (: (.. "%." d "f") :format n)
							(s) (raw:gsub "%.?0+$" "")
							num (tonumber s)]
					(if cached-format-num
							(cached-format-num num)
							(tostring num))))))

(fn extract-prefix [pre-text]
	(var last "")
	(each [name (: (tostring (or pre-text "")) :gmatch "|%s*([^|\n]+)%s*")]
		(let [raw (trim name)]
			(when (and (not (raw:match "^%-*$")) (not (raw:match "=%s*$")))
				(let [(n) (raw:gsub "[^%a%d]+" "")]
					(when (and (not= n "") (not= (n:lower) "tabber"))
						(set last n))))))
	(when (= last "")
		(each [name (: (tostring (or pre-text ""))
									 :gmatch "%-?%-%s*|?%s*([^|\n<>={}\t/]+)%s*=%s*")]
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
			"<span class=\"mw-collapsible\" id=\"mw-customcollapsible-"
			cell-id "-on\">" v-rof "</span>"))

(fn history-page? []
	;; never show .open-se on /History
	;; not until I add historical data support for SE, at least
	(let [title (and mw mw.title (mw.title.getCurrentTitle))
				page-name (or (and title title.text) "")]
		(and (not= page-name "")
				 (page-name:find "/History" 1 true)
				 true)))

(fn make-editor-btn-html [is-fse]
	(if (or (not is-fse) (history-page?))
			nil
			(let [title (and mw mw.title (mw.title.getCurrentTitle))
						page-name (or (and title title.text) "TestPage")
						enc (if (and mw mw.uri mw.uri.encode)
										(mw.uri.encode page-name "PATH")
										page-name)]
				;; see [[MediaWiki:Shared.css]] for class
				(.. "<div class=\"wds-button open-se\">"
						"[https://se.tds.wiki/tower/" enc
						" Open in Statistics Editor]"
						"</div>"))))

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
									label-id "-off\">Enable Rate of Fire Bug</span>"
									"</div>")
					editor (or (make-editor-btn-html is-fse) "")
					body (.. "<div>" table-out "</div>")]
			(.. "<div class=\"rofbug-wrapper\">"
					btn
					editor
					body
					"</div>"))))

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
				(or (. formula-env base)
						(= base "FNC-TOTALPRICE")
						(not= (base:match "^FNC%-TOTAL%-(.+)$") nil)))
			false))

;; free fn pcall(f, ...) so we do not alloc a closure per $VAR$
(fn eval-known-var [name ctx formula-env parse-cache]
	(eval.eval-node ctx (parser.parse-var name formula-env parse-cache [])))

(fn eval-var-name [name ctx formula-env parse-cache]
	;; unknown -> nil (leave token)
	;; known still pcall so one bad cell
	;; (N/A, wikilinks) does not abort the whole page
	(if (not (formula-name-evaluable? name formula-env))
			nil
			(let [(ok result) (pcall eval-known-var name ctx formula-env parse-cache)]
				(if ok result nil))))

(fn expand-dollars [text ctx formula-env parse-cache]
	;; recurse into string results so gsub does not rescan replacements
	(fn expand [s depth]
		(when (> depth 12)
			(error "expand-dollars: nesting too deep"))
		(pick-values 1
			(: (tostring (or s "")) :gsub "%$([^%$]+)%$"
				 (fn [name]
					 (let [r (eval-var-name name ctx formula-env parse-cache)]
						 (if (= r nil)
								 (.. "$" name "$")
								 (= (type r) :number)
								 (fmt r)
								 (let [str (tostring r)]
									 ;; "$", not pattern "%$"
									 (if (str:find "$" 1 true)
											 (expand str (+ depth 1))
											 str))))))))
	(expand text 0))

(fn coerce-row-value [v]
	(if (= (type v) :number)
			v
			(let [s (tostring (or v ""))
						n (tonumber s)
						m (and (not n) (s:match "[%d,]+%.?%d*"))]
				(or n
						(and m (tonumber (pick-values 1 (m:gsub "," ""))))
						v))))

(fn expand-inline-expr [text frame]
	(pick-values 1
		(: (tostring (or text "")) :gsub "{{#expr:(.-)}}"
			 (fn [body]
				 (tostring (or (frame:callParserFunction {:name "#expr" :args [body]})
											 ""))))))

(fn preprocess-if-needed [s frame]
	(let [text (tostring (or s ""))]
		(if (not (text:find "{{#expr:" 1 true))
				text
				(and frame frame.callParserFunction)
				(expand-inline-expr text frame)
				(and frame frame.preprocess)
				(frame:preprocess text)
				text)))

(fn detect-table-branch [tbl branch-map]
	(var branch "")
	(var done false)
	(each [line (: (.. tbl "\n") :gmatch "([^\n]*)\n") &until done]
		(let [t (trim line)]
			(when (and (t:match "^!") (: (t:lower) :match "colspan"))
				(let [name (t:match "%|(.+)$")]
					(when name
						(let [bn (trim name)]
							(set branch (or (and branch-map (. branch-map bn))
															(and branch-map (. branch-map (bn:gsub "%s+" "")))
															"")))))
				(set done true))))
	branch)

(fn make-eval-base [vars prefix formula-env parse-cache frame table-cache
										branch ?branch-map]
	;; COST/SCHEMA once per table not per cell.
	(let [prefix (or prefix "")
				costs (config.parse-number-list
								(or (. vars (config.get-var-key vars prefix "COST")) ""))
				schema (config.parse-schema
								 (. vars (config.get-var-key vars prefix "SCHEMA")))
				bmap (or ?branch-map {})
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
		 :table-cache table-cache
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

(fn set-row-field! [row header value]
	(set (. row header) value)
	(let [stripped (pick-values 1 (header:gsub "%s+" ""))]
		(when (not= stripped header)
			(set (. row stripped) value)))
	value)

(fn process-table [tbl vars formula-env parse-cache rof-cols rof-offset
									 branch-map frame builder table-cache prefix]
	(let [headers []
				out []
				recursion {}
				rof-cols (or rof-cols {})
				has-rof (next rof-cols)
				branch (detect-table-branch tbl branch-map)
				base (make-eval-base vars prefix formula-env parse-cache frame
														 table-cache branch branch-map)
				ctx-n (build-eval-ctx base {} false 0)
				ctx-r (when has-rof (build-eval-ctx base {} true 0))
				rc-aliases (collect [k v (pairs vars)]
										 (when (= (trim v) "$FNC-RECURSION$")
											 (values k true)))
				has-rc (next rc-aliases)]
		(var level -1)
		(var col-idx 1)
		(var row-norm {})
		(var row-rof {})

		(fn expand-rc [s]
			(pick-values 1
				(s:gsub "%$([^%$]+)%$"
								(fn [name]
									(if (. rc-aliases (.. "$" name "$"))
											"$FNC-RECURSION$"
											(.. "$" name "$"))))))

		(each [line (: (.. tbl "\n") :gmatch "([^\n]*)\n")]
			(let [t (trim line)]
				(if (t:match "^|%-")
						(do
							(set col-idx 1)
							(set row-norm {})
							(set row-rof {})
							(when (>= level 0)
								(set-row-field! row-norm "Level" level)
								(set-row-field! row-rof "Level" level))
							(table.insert out line))
						(and (t:match "^!") (not (: (t:lower) :match "colspan")))
						(do
							(let [body (t:sub 2)
										(norm) (body:gsub "%|%|" "!!")]
								(each [cell (: (.. norm "!!") :gmatch "([^!]*)!!")]
									(let [hc (trim cell)]
										(when (not= hc "")
											(let [temp (expand-dollars hc ctx-n formula-env
																								 parse-cache)
														clean (wikitable.clean-header-text temp)]
												(when (not= clean "")
													(table.insert headers clean)))))))
							(table.insert out line))
						(and (t:match "^|") (not (t:match "^|}")))
						(do
							(local cells (wikitable.split-row-cells t))
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
								(let [h (. headers col-idx)]
									(when (= h "Level")
										(let [keys (wikitable.parse-level-keys cell)]
											(if (> (length keys) 0)
													(let [k0 (. keys 1)
																k0s (tostring k0)
																from-str (k0s:match "^%d+")]
														(set level (or (and (= (type k0) :number) k0)
																					 (and from-str (tonumber from-str))
																					 (+ level 1))))
													(set level (or (tonumber cell) (+ level 1))))
											(set-row-field! row-norm "Level" level)
											(set-row-field! row-rof "Level" level)))
									(var v-norm cell)
									(var v-rof cell)
									(when (cell:match "%$[^%$]+%$")
										;; dual expand only with ROF as formulas may read things like Firerate_ROF
										(eval.bind-ctx! ctx-n row-norm false level)
										(set v-norm (expand-dollars cell ctx-n formula-env
																								parse-cache))
										(if has-rof
												(do
													(eval.bind-ctx! ctx-r row-rof true level)
													(set v-rof (expand-dollars cell ctx-r formula-env
																										 parse-cache)))
												(set v-rof v-norm)))
									(set v-norm (preprocess-if-needed v-norm frame))
									(set v-rof (if has-rof
																 (preprocess-if-needed v-rof frame)
																 v-norm))
									(when h
										(set-row-field! row-norm h (coerce-row-value v-norm))
										(set-row-field! row-rof h
																		(if has-rof
																				(coerce-row-value v-rof)
																				(. row-norm h))))
									(when (and has-rof h (. rof-cols h))
										(let [n (or (tonumber v-norm)
																(tonumber (v-norm:match "%d+%.?%d*")))]
											(when n
												(let [rv (tablecache.rof-bug n rof-offset)]
													(set-row-field! row-rof h rv)
													(set v-rof (fmt rv 3))))))
									(if (and has-rof (not= (tostring v-norm) (tostring v-rof)))
											(let [cell-id (next-cell-toggle! builder)]
												(set (. cells ci)
														 (rof-cell-html cell-id
																						(tostring v-norm)
																						(tostring v-rof))))
											(set (. cells ci) (tostring v-norm)))
									(set col-idx (+ col-idx 1))))
							(table.insert out (.. "|" (table.concat cells "||"))))
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
														 opts.frame (or opts.table-cache {})
														 "" (or opts.branch-map {}))
				ctx (build-eval-ctx base {} false 0)]
		(each [name (o:gmatch "%$([^%$]+)%$")]
			(let [tok (.. "$" name "$")]
				(set (. token-count tok) (+ (or (. token-count tok) 0) 1))))
		(pick-values 1
			(o:gsub "%$([^%$]+)%$"
							(fn [name]
								(let [tok (.. "$" name "$")
											r (eval-var-name name ctx formula-env parse-cache)]
									(if (= r nil)
											;; unevaluable FNC-* config keys are not display text
											(if (name:match "^FNC%-") "" tok)
											(= (type r) :number)
											(fmt r)
											(do
												(local str (tostring r))
												(let [(ref-attrs ref-content)
															(str:match "^%s*<ref%s*([^>]*)>(.-)</ref>%s*$")]
													(if (and ref-attrs (> (or (. token-count tok) 0) 1))
															(let [bare (pick-values 1
																					 (name:gsub "[^%a%d]" ""))
																		ref-name (or (get-attr ref-attrs "name")
																								 (bare:lower))
																		ref-group (get-attr ref-attrs "group")
																		group-attr (if ref-group
																									 (.. " group=\"" ref-group "\"")
																									 "")
																		count (+ (or (. ref-seen tok) 0) 1)]
																(set (. ref-seen tok) count)
																(if (= count 1)
																		(.. "<ref" group-attr " name=\"" ref-name
																				"\">" ref-content "</ref>")
																		(.. "<ref" group-attr " name=\"" ref-name
																				"\"/>")))
															(if (str:find "$" 1 true)
																	(expand-dollars str ctx formula-env
																									parse-cache)
																	str)))))))))))

(fn build-out [content vars cfg frame builder]
	(let [formula-env (or (and cfg cfg.formula-env) (config.formula-env vars))
				parse-cache {}
				rof-cols (or (and cfg cfg.rof-cols) {})
				rof-offset (and cfg cfg.rof-offset)
				prefix0 (or (and cfg cfg.prefix) "")
				page-bmap (or (and cfg cfg.branch-map) {})
				spans (wikitable.find-table-spans content)
				parsed (icollect [_ span (ipairs spans)]
								 (wikitable.parse-table span.text page-bmap))
				page-cache (tablecache.build-page-cache
										 parsed
										 {:prefix prefix0
											:index-overrides (or (and cfg cfg.index) [])
											:rof-cols rof-cols
											:rof-offset rof-offset})
				result []
				n (length content)]
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
				;; free text kept raw until finalize-output (needs ref counts)
				(when (not= pre "")
					(table.insert result pre))
				(let [branch-map (config.build-branch-map vars prefix)
							processed (process-table span.text vars formula-env
																			 parse-cache rof-cols
																			 rof-offset branch-map
																			 frame builder page-cache
																			 prefix)]
					(table.insert result processed))
				(set cursor (+ span.stop 1))))
		(when (<= cursor n)
			(table.insert result (content:sub cursor)))
		(trim (finalize-output (table.concat result) vars formula-env parse-cache
													 {:frame frame
														:table-cache page-cache
														:prefix prefix0
														:branch-map page-bmap}))))

(fn strip-var-blocks [content]
	(pick-values 1
		(: (tostring (or content "")) :gsub "[ \t]*<var>.-</var>[ \t]*\n?" "")))

(fn render-page [content frame ?opts]
	(let [opts (or ?opts {})
				_ (do
						(set cached-format-num false)
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
