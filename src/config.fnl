;; FNC/FSE page config

(fn trim [s]
	(let [str (tostring (or s ""))]
		(pick-values 1 (str:match "^%s*(.-)%s*$"))))

(fn split-lines [s]
	(let [out []
				text (tostring (or s ""))]
		(var start 1)
		(for [i 1 (length text)]
			(when (= (text:sub i i) "\n")
				(table.insert out (text:sub start (- i 1)))
				(set start (+ i 1))))
		(table.insert out (text:sub start))
		out))

(fn split-semi [raw]
	(let [out []
				s (tostring (or raw ""))]
		(if (= s "")
				out
				(do
					(var start 1)
					(for [i 1 (length s)]
						(when (= (s:sub i i) ";")
							(table.insert out (trim (s:sub start (- i 1))))
							(set start (+ i 1))))
					(table.insert out (trim (s:sub start)))
					out))))

(fn strip-var-key [k]
	(or (: (tostring k) :match "^%$(.*)%$$") (tostring k)))

(fn dollar-key [name]
	(.. "$" name "$"))

(fn config-name? [name]
	(or (name:match "^FNC%-")
			(name:match "^FSE%-")
			(name:match "^ROFBUG")))

(fn formula-name? [name]
	(not (config-name? name)))

(fn parse-vars [text]
	(let [vars {}]
		(var is-fse false)
		(each [block (: (tostring (or text "")) :gmatch "<var>(.-)</var>")]
			(let [lines (split-lines block)]
				(var i 1)
				(while (<= i (length lines))
					(let [t (trim (. lines i))]
						(when (not= t "")
							(let [eq (t:find "=" 1 true)]
								(when eq
									(var k (trim (t:sub 1 (- eq 1))))
									(var v (trim (t:sub (+ eq 1))))
									(let [q (v:sub 1 1)]
										(if (and (or (= q "\"") (= q "'"))
														 (or (= v q) (not= (v:sub -1) q)))
												(do
													(var parts [(v:sub 2)])
													(var done false)
													(while (and (not done) (< i (length lines)))
														(set i (+ i 1))
														(table.insert parts (. lines i))
														(when (= (: (trim (. lines i)) :sub -1) q)
															(let [joined (table.concat parts "\n")
																		close-at (joined:find (.. q "[^" q "]*$"))]
																(when close-at
																	(set v (joined:sub 1 (- close-at 1))))
																(set done true)))))
												(or (= q "\"") (= q "'"))
												(set v (v:sub 2 -2))))
									(when (not (k:match "^%$.*%$$"))
										(set k (dollar-key k)))
									(tset vars k v)
									(when (and (not is-fse) (k:find "FSE-" 1 true))
										(set is-fse true))))))
					(set i (+ i 1)))))
		(values vars is-fse)))

(fn get-var-key [vars prefix suffix]
	(let [prefix (or prefix "")]
		(if (not= prefix "")
				(let [k (dollar-key (.. "FNC-" prefix "-" suffix))]
					(if (not= (. vars k) nil)
							k
							(dollar-key (.. "FNC-" suffix))))
				(dollar-key (.. "FNC-" suffix)))))

(fn get-array-var-key [vars prefix name]
	(let [prefix (or prefix "")]
		(if (not= prefix "")
				(let [k (dollar-key (.. prefix "-" name))]
					(if (not= (. vars k) nil)
							k
							(dollar-key name)))
				(dollar-key name))))

(fn strip-refs [raw]
	(var body (tostring (or raw "")))
	(set body (pick-values 1 (body:gsub "<ref[^>]*>.-</ref>" "")))
	(set body (pick-values 1 (body:gsub "<ref[^>]*/>" "")))
	(trim body))

(fn is-numeric-array-body [raw]
	;; empty is false so missing TOTAL-X series will hard fail instead of summing 0
	(let [body (strip-refs raw)
				lower (body:lower)]
		(if (= body "")
				false
				(or (body:find "{{" 1 true) (lower:find "#expr" 1 true))
				false
				(not (body:find ";" 1 true))
				(not= (tonumber (pick-values 1 (body:gsub "," ""))) nil)
				(do
					(var ok true)
					(each [_ part (ipairs (split-semi body)) &until (not ok)]
						(let [t (trim part)]
							(when (and (not= t "")
												 (= (tonumber (pick-values 1 (t:gsub "," ""))) nil))
								(set ok false))))
					ok))))

(fn parse-number-list [raw]
	;; blank slots -> 0 are schema gaps
	;; non-empty garbage errors (NS0 no silent 0)
	(icollect [_ part (ipairs (split-semi raw))]
		(let [t (trim part)]
			(if (= t "")
					0
					(let [(cleaned) (t:gsub "," "")
								n (tonumber cleaned)]
						(when (not n)
							(error (.. "config: not a number in list: `" t "`")))
						n)))))

(fn parse-string-list [raw]
	(icollect [_ part (ipairs (split-semi raw))]
		(let [t (trim part)]
			(when (not= t "") t))))

(fn parse-schema [raw]
	(when (and raw (not= (trim raw) ""))
		(icollect [_ part (ipairs (split-semi raw))]
			(trim part))))

(fn build-branch-map [vars prefix]
	(let [schema-str (. vars (get-var-key vars prefix "SCHEMA"))]
		(if (not schema-str)
				{}
				(let [schema (or (parse-schema schema-str) [])
							trunk (or (. schema 1) "N")
							seen {}
							branch-letters []]
					(each [_ letter (ipairs schema)]
						(when (and (not= letter trunk) (not (. seen letter)))
							(tset seen letter true)
							(table.insert branch-letters letter)))
					(let [branch-key (get-var-key vars prefix "BRANCH")
								branch-map {}]
						(var bi 1)
						(each [_ name (ipairs (parse-string-list (or (. vars branch-key) "")))]
							(when (. branch-letters bi)
								(tset branch-map name (. branch-letters bi))
								(tset branch-map (name:gsub "%s+" "") (. branch-letters bi))
								(set bi (+ bi 1))))
						branch-map)))))

(fn parse-index-overrides [raw]
	(icollect [_ part (ipairs (parse-string-list (or raw "")))]
		(let [(tname col) (part:match "^(.-)%.(.+)$")]
			(if tname
					{:table (trim tname) :col (trim col)}
					{:table (trim part) :col nil}))))

(fn parse-rofbug [vars]
	;; prefer dated FNC-ROFBUG-*
	;; 2019/2020 carry fixed offsets
	(let [raw (if (. vars "$FNC-ROFBUG-2019$")
								(. vars "$FNC-ROFBUG-2019$")
								(. vars "$FNC-ROFBUG-2020$")
								(. vars "$FNC-ROFBUG-2020$")
								(or (. vars "$FNC-ROFBUG-2022$")
										(. vars "$FNC-ROFBUG$")
										(. vars "$ROFBUG$")
										(. vars "$ROFBUG-2022$")
										(. vars "$ROFBUG-2020$")
										(. vars "$ROFBUG-2019$")))
				offset (if (. vars "$FNC-ROFBUG-2019$") 0.05
									 (. vars "$FNC-ROFBUG-2020$") 0.03
									 nil)
				cols {}]
		(each [_ c (ipairs (parse-string-list (or raw "")))]
			(tset cols c true))
		(values cols offset)))

(fn collect-fse [vars]
	(collect [k v (pairs vars)]
		(let [name (strip-var-key k)
					rest (name:match "^FSE%-(.+)$")]
			(when rest
				(values rest v)))))

(fn formula-env [vars]
	;; FNC/FSE should stay out
	;; TOTAL* are eval intrinsics and not arithmetic formulas
	(collect [k v (pairs vars)]
		(let [name (strip-var-key k)]
			(when (formula-name? name)
				(values name v)))))

(fn build-config [vars prefix]
	(let [prefix (or prefix "")
				cost-key (get-var-key vars prefix "COST")
				schema-key (get-var-key vars prefix "SCHEMA")
				index-key (get-var-key vars prefix "INDEX")
				(rof-cols rof-offset) (parse-rofbug vars)
				schema (parse-schema (. vars schema-key))]
		{:vars vars
		 :prefix prefix
		 :costs (parse-number-list (or (. vars cost-key) ""))
		 :cost-key cost-key
		 :schema schema
		 :branch-map (build-branch-map vars prefix)
		 :index (parse-index-overrides (. vars index-key))
		 :rof-cols rof-cols
		 :rof-offset rof-offset
		 :fse (collect-fse vars)
		 :formula-env (formula-env vars)}))

(fn parse-page-config [text prefix]
	(let [(vars is-fse) (parse-vars text)
				cfg (build-config vars prefix)]
		(values cfg is-fse)))

{:trim trim
 :split-semi split-semi
 :split-lines split-lines
 :strip-var-key strip-var-key
 :dollar-key dollar-key
 :config-name? config-name?
 :formula-name? formula-name?
 :parse-vars parse-vars
 :get-var-key get-var-key
 :get-array-var-key get-array-var-key
 :is-numeric-array-body is-numeric-array-body
 :parse-number-list parse-number-list
 :parse-string-list parse-string-list
 :parse-schema parse-schema
 :build-branch-map build-branch-map
 :parse-index-overrides parse-index-overrides
 :parse-rofbug parse-rofbug
 :formula-env formula-env
 :build-config build-config
 :parse-page-config parse-page-config}
