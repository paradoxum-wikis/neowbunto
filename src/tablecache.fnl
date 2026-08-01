;; Level -> row table cache

(local {: parse-level-keys : strip-cell-refs : index-col : parse-all-tables}
			 (require :wikitable))

(fn cache-get [cache level branch]
	;; Table.Col lookups hit here by level
	;; + optional branch letter
	(when cache
		(if (and branch (not= branch ""))
				(or (. cache (.. (tostring level) branch))
						(. cache level))
				(. cache level))))

(fn rof-bug [n offset]
	;; 2019 / 2020 are fixed offset, not sure how true it is
	;; but Mentin said so, so we'll go with that
	(if (<= n 0)
			n
			offset
			(+ n offset)
			(let [raw (* n 60)
						fr (math.floor (+ raw 0.5))
						frames (if (< (math.abs (- raw fr)) 1e-9)
											 (+ fr 1.5)
											 (+ (math.ceil raw) 1))]
				(/ (math.floor (+ (* (/ frames 60) 1000) 0.5)) 1000))))

(fn coerce-cell [raw]
	(let [clean (strip-cell-refs raw)
				n (tonumber clean)]
		(if (not= n nil) n clean)))

(fn strip-html [s]
	(var t (tostring (or s "")))
	(set t (pick-values 1 (t:gsub "<ref[^>]*>.-</ref>" "")))
	(set t (pick-values 1 (t:gsub "<ref[^>]*/>" "")))
	(set t (pick-values 1 (t:gsub "<[^>]+>" "")))
	(pick-values 1 (t:match "^%s*(.-)%s*$")))

(fn apply-resolve [resolve-cell raw header row-so-far rof?]
	(let [v (if resolve-cell
							(resolve-cell raw header row-so-far rof?)
							(coerce-cell raw))]
		(if (= (type v) :number)
				v
				(let [cleaned (strip-html (tostring v))
							n (tonumber cleaned)]
					(if (not= n nil) n cleaned)))))

(fn set-row-field [row header value]
	(set (. row header) value)
	(let [(stripped) (header:gsub "%s+" "")]
		(set (. row stripped) value))
	value)

(fn extract-number [v]
	(if (= (type v) :number)
			v
			(let [n (tonumber v)]
				(if (not= n nil)
						n
						(let [s (tostring (or v ""))
									m (s:match "%-?%d+%.?%d*")]
							(and m (tonumber m)))))))

(fn build-table-cache [table-ast index-col opts]
	;; ROF values live as Header_ROF on the same entry
	(let [opts (or opts {})
				resolve-cell opts.resolve-cell
				rof-cols (or opts.rof-cols {})
				rof-offset opts.rof-offset
				rof-fn (or opts.rof-bug rof-bug)
				headers (or table-ast.headers [])
				idx (or index-col (. headers 1))
				cache {}]
		(each [_ row (ipairs (or table-ast.rows []))]
			(let [row-norm {}
						row-rof {}
						raw-cells (or row.raw-cells [])]
				(each [i header (ipairs headers)]
					(when (<= i (length raw-cells))
						(let [raw (. raw-cells i)
									res-norm (apply-resolve resolve-cell raw header row-norm false)
									res-rof (apply-resolve resolve-cell raw header row-rof true)]
							(set-row-field row-norm header res-norm)
							(var rof-val res-rof)
							(when (. rof-cols header)
								(let [n (extract-number res-norm)]
									(when n
										(set rof-val (rof-fn n rof-offset)))))
							(set-row-field row-rof header rof-val))))
				(let [index-val (when idx
													(or (. row-norm idx)
															(. row-norm (idx:gsub "%s+" ""))))]
					(when (not= index-val nil)
						(each [_ key (ipairs (parse-level-keys index-val))]
							(let [entry {}]
								(each [k v (pairs row-norm)]
									(set (. entry k) v))
								(each [k v (pairs row-rof)]
									(set (. entry (.. k "_ROF")) v))
								(when (= (type key) :number)
									(set entry.Level key)
									(when (= (. entry "Level_ROF") nil)
										(set entry.Level_ROF key)))
								(set (. cache key) entry)))))))
		cache))

(fn register-cache [page-cache table-name cache prefix]
	;; same title under Regular/PVP
	;; bare key keeps first while rest is name|Prefix
	(let [prefix (or prefix "")
				key (if (not= prefix "")
								(.. table-name "|" prefix)
								table-name)]
		(set (. page-cache key) cache)
		(set (. page-cache (key:gsub "%s+" "")) cache)
		(when (not (. page-cache table-name))
			(set (. page-cache table-name) cache))
		(let [(stripped) (table-name:gsub "%s+" "")]
			(when (not (. page-cache stripped))
				(set (. page-cache stripped) cache)))
		page-cache))

(fn scope-page-cache [page-cache prefix]
	;; collapse Name|tab onto bare Name for this tab
	(when page-cache
		(let [prefix (or prefix "")]
			(if (= prefix "")
					page-cache
					(let [view {}
								suffix (.. "|" prefix)
								slen (length suffix)]
						(each [k v (pairs page-cache)]
							(when (and (= (type k) :string) (not (k:find "|" 1 true)))
								(set (. view k) v)))
						(each [k v (pairs page-cache)]
							(when (and (= (type k) :string)
												 (> (length k) slen)
												 (= (k:sub (- slen)) suffix))
								(let [base (k:sub 1 (- (length k) slen))]
									(set (. view base) v)
									(let [(stripped) (base:gsub "%s+" "")]
										(when (not= stripped base)
											(set (. view stripped) v))))))
						view)))))

(fn table-name-of [table-ast fallback-index]
	(or table-ast.title
			(and fallback-index (.. "Table" (tostring fallback-index)))
			"Table"))

(fn build-page-cache [tables opts]
	(let [opts (or opts {})
				page {}
				index-overrides opts.index-overrides
				default-prefix (or opts.prefix "")
				table-prefixes (or opts.table-prefixes [])]
		(each [i table-ast (ipairs (or tables []))]
			(let [name (table-name-of table-ast i)
						idx (index-col table-ast index-overrides)
						cache (build-table-cache table-ast idx opts)
						pfx (or (. table-prefixes i) default-prefix "")]
				(register-cache page name cache pfx)))
		page))

(fn build-page-cache-from-text [content branch-map opts]
	(build-page-cache (parse-all-tables content (or branch-map {})) opts))

{:cache-get cache-get
 :rof-bug rof-bug
 :coerce-cell coerce-cell
 :build-table-cache build-table-cache
 :build-page-cache build-page-cache
 :build-page-cache-from-text build-page-cache-from-text
 :register-cache register-cache
 :scope-page-cache scope-page-cache}
