;; tower fixture smoke
;; fails on throw / empty / leftover $VAR$ / leftover <var>

(local {: render-page} (require :render))

(fn read-file [path]
	(let [f (io.open path "r")]
		(when (not f)
			(error (.. "corpus: cannot open " path)))
		(let [s (f:read "*a")]
			(f:close)
			s)))

(fn list-towers []
	(let [f (io.open "tests/towers/manifest.txt" "r")]
		(if (not f)
				[]
				(let [names []]
					(each [line (f:lines)]
						(let [n (line:match "^%s*(.-)%s*$")]
							(when (and n (not= n "") (n:match "%.wiki$"))
								(table.insert names n))))
					(f:close)
					names))))

(fn first-leftover-var [s]
	(s:match "%$([A-Za-z][%w%-]*)%$"))

(fn smoke-one [name]
	(let [path (.. "tests/towers/" name)
				raw (read-file path)
				(ok result) (pcall render-page raw nil {:seed 1})]
		(if ok
				(values true nil result)
				(values false (tostring result) nil))))

(fn classify-output [name out failures]
	(if (or (not out) (= (or (out:match "^%s*(.-)%s*$") "") ""))
			(do
				(table.insert failures [name "empty output"])
				:empty)
			(out:find "<var>" 1 true)
			(do
				(table.insert failures [name "output still contains <var>"])
				:varblock)
			(let [leftover (first-leftover-var out)]
				(if leftover
						(do
							(table.insert failures
														 [name (.. "unresolved $" leftover "$")])
							:leftover)
						:ok))))

(fn run []
	(let [names (list-towers)
				failures []]
		(var ok-n 0)
		(var fail-n 0)
		(var empty-n 0)
		(var leftover-n 0)
		(var varblock-n 0)
		(print (.. "  corpus: " (tostring (length names)) " tower fixtures"))
		(when (= (length names) 0)
			(print "  corpus: empty tests/towers/manifest.txt is a failure")
			(os.exit 1))
		(each [_ name (ipairs names)]
			(let [(ok err out) (smoke-one name)]
				(if (not ok)
						(do
							(set fail-n (+ fail-n 1))
							(table.insert failures [name err]))
						(let [kind (classify-output name out failures)]
							(case kind
								:ok (set ok-n (+ ok-n 1))
								:empty (set empty-n (+ empty-n 1))
								:varblock (set varblock-n (+ varblock-n 1))
								:leftover (set leftover-n (+ leftover-n 1)))))))
		(print (.. "  ok=" ok-n
							 "  fail=" fail-n
							 "  empty=" empty-n
							 "  leftover_VAR=" leftover-n
							 "  leftover_varblock=" varblock-n))
		(when (> (length failures) 0)
			(print "  failures:")
			(var shown 0)
			(each [_ pair (ipairs failures)]
				(when (< shown 40)
					(let [nm (. pair 1)
								msg (. pair 2)
								short (or (and msg (msg:match "([^\n]+)")) msg)]
						(print (.. "    - " nm ": " (or short "?")))
						(set shown (+ shown 1)))))
			(when (> (length failures) 40)
				(print (.. "    ... +" (tostring (- (length failures) 40))
									 " more"))))
		(let [bad (+ fail-n empty-n leftover-n varblock-n)]
			(when (> bad 0)
				(os.exit 1))
			{:ok ok-n
			 :fail fail-n
			 :empty empty-n
			 :leftover leftover-n
			 :varblock varblock-n
			 :failures failures})))

{:run run
 :smoke-one smoke-one
 :list-towers list-towers
 :first-leftover-var first-leftover-var}
