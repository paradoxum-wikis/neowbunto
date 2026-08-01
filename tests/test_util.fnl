;; assertion helpers for unit suites

(var passed 0)
(var failed 0)
(var current-suite "")

(fn deep-eq? [a b]
	(if (= a b)
		true
		(not= (type a) (type b))
		false
		(not= (type a) :table)
		false
		(do
		(var ok true)
		(each [k v (pairs a) &until (not ok)]
			(when (not (deep-eq? v (. b k)))
				(set ok false)))
		(each [k _v (pairs b) &until (not ok)]
			(when (= (. a k) nil)
				(set ok false)))
		ok)))

(fn sequential? [t]
	(var i 1)
	(var ok true)
	(each [k _ (pairs t) &until (not ok)]
		(if (not= k i)
			(set ok false)
			(set i (+ i 1))))
	(and ok (> i 1)))

(fn fmt-val [v]
	(if (and (= (type v) :table) (sequential? v))
		(let [parts []]
		(each [_ x (ipairs v)]
			(table.insert parts (fmt-val x)))
		(.. "[" (table.concat parts " ") "]"))
		(= (type v) :table)
		(let [parts []]
		(each [k x (pairs v)]
			(table.insert parts (.. (tostring k) "=" (fmt-val x))))
		(.. "{" (table.concat parts " ") "}"))
		(= (type v) :string)
		(.. "\"" v "\"")
		(tostring v)))

(fn suite [name]
	(set current-suite name)
	(print (.. "  " name)))

(fn assert-eq [actual expected msg]
	(if (deep-eq? actual expected)
		(set passed (+ passed 1))
		(do
		(set failed (+ failed 1))
		(print (.. "FAIL [" current-suite "] " (or msg "")))
		(print (.. "  expected: " (fmt-val expected)))
		(print (.. "  actual:   " (fmt-val actual))))))

(fn assert-true [cond msg]
	(if cond
		(set passed (+ passed 1))
		(do
		(set failed (+ failed 1))
		(print (.. "FAIL [" current-suite "] " (or msg "")))
		(print "  expected: true")
		(print (.. "  actual:   " (tostring cond))))))

(fn assert-near [actual expected tol msg]
	(let [t (or tol 1e-9)]
		(if (and (= (type actual) :number)
				 (= (type expected) :number)
				 (<= (math.abs (- actual expected)) t))
			(set passed (+ passed 1))
			(do
			(set failed (+ failed 1))
			(print (.. "FAIL [" current-suite "] " (or msg "")))
			(print (.. "  expected ≈ " (tostring expected) " ± " (tostring t)))
			(print (.. "  actual:    " (tostring actual)))))))

(fn assert-error [f msg-substr]
	(let [(ok err) (pcall f)]
		(if ok
			(do
			(set failed (+ failed 1))
			(print (.. "FAIL [" current-suite "] expected error"
				   (if msg-substr (.. " matching " msg-substr) ""))))
			(and msg-substr (not (: (tostring err) :find msg-substr 1 true)))
			(do
			(set failed (+ failed 1))
			(print (.. "FAIL [" current-suite "] error message mismatch"))
			(print (.. "  expected substring: " msg-substr))
			(print (.. "  actual: " (tostring err))))
			(set passed (+ passed 1)))))

(fn summary []
	(print (.. "\n" passed " passed, " failed " failed"))
	(when (> failed 0)
		(os.exit 1))
	true)

{:suite suite
 :assert-eq assert-eq
 :assert-true assert-true
 :assert-near assert-near
 :assert-error assert-error
 :summary summary
 :deep-eq? deep-eq?}
