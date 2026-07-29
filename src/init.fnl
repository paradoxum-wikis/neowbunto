;; Scribunto entry is Template:Neow -> {{#invoke:Neowbunto|heeho|{{{1}}}}}

(local {: render-page} (require :render))

(fn trim [s]
	(pick-values 1 (: (tostring (or s "")) :match "^%s*(.-)%s*$")))

(fn content-arg [frame]
	;; body is always invoke arg #1 after {{{1}}}
	;; there are no parent-frame merge
	(let [args (and frame frame.args)
				raw (and args (. args 1))]
		(if (= raw nil)
				""
				(trim raw))))

(fn unescape-entities [s]
	;; &amp last parity with the current Neowbunto written in Lua
	;; may change later, hopefully I do not forget
	(var t (tostring (or s "")))
	(set t (pick-values 1 (t:gsub "&lt;" "<")))
	(set t (pick-values 1 (t:gsub "&gt;" ">")))
	(set t (pick-values 1 (t:gsub "&amp;" "&")))
	t)

(fn prepare-content [raw]
	(let [unstrip (or (and mw mw.text mw.text.unstripNoWiki)
										(fn [x] x))
				text (unstrip (or raw ""))]
		(unescape-entities text)))

(fn preprocess [frame out]
	(if (and frame frame.preprocess)
			(frame:preprocess out)
			out))

(local p {})

(fn p.heeho [frame]
	(let [raw (content-arg frame)
				content (prepare-content raw)]
		(if (or (not content) (= content ""))
				"'''Neowbunto''': No valid content found."
				(preprocess frame (render-page content frame)))))

(tset p :content-arg content-arg)

p
