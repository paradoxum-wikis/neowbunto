;; local Scribunto mw + frame mock
;; #expr goes through mwexpr (same language offline)
;; callParserFunction accepts name+args, varargs, or { name, args }

(fn unstrip-no-wiki [s]
	s)

(fn get-content-language []
	{:formatNum (fn [_self n] (tostring n))})

(fn create-html [tag]
	(local children [])
	(local classes [])
	(local attrs {})
	(var text "")
	(let [node {}]
		(fn node.addClass [_ c]
			(table.insert classes c)
			node)
		(fn node.css [_ k v]
			(set (. attrs k) v)
			node)
		(fn node.attr [_ k v]
			(set (. attrs k) v)
			node)
		(fn node.wikitext [_ t]
			(set text (.. text (tostring (or t ""))))
			node)
		(fn node.done [_]
			node)
		(fn node.tag [_ t]
			(let [child (create-html t)]
				(table.insert children child)
				child))
		(fn node.tostring [_]
			(.. "<" tag ">" text
				(table.concat (icollect [_ c (ipairs children)]
								(c:tostring))
							  "")
				"</" tag ">"))
		node))

(fn make-frame [args]
	(let [frame {:args (or args {})}]
		(fn frame.getParent [_]
			nil)
		(fn frame.callParserFunction [_self a b ...]
			(var name nil)
			(var expr nil)
			(if (and (= (type a) :table) a.name)
				(do
				  (set name a.name)
				  (set expr (or (and a.args (. a.args 1)) a.args)))
				(= (type b) :table)
				(do
				  (set name a)
				  (set expr (or (. b 1) (. b ""))))
				(do
				  (set name a)
				  (set expr b)))
			(when (= (type expr) :table)
				(set expr (or (. expr 1) (. expr ""))))
			(if (or (= name "#expr") (= name "expr") (= name "expr:"))
				(let [mwexpr (require :mwexpr)]
				  ;; Scribunto does not preprocess PF args
				  (tostring (mwexpr.eval (tostring (or expr "0")) {})))
				""))
		(fn frame.preprocess [self s]
			(let [text (tostring (or s ""))]
				(pick-values 1
					(text:gsub "{{#expr:(.-)}}"
							   (fn [body]
								 (self:callParserFunction {:name "#expr"
														   :args [body]}))))))
		frame))

(set _G.mw
	  {:text {:unstripNoWiki unstrip-no-wiki}
	   :language {:getContentLanguage get-content-language}
	   :html {:create create-html}
	   :title {:getCurrentTitle (fn [] {:text "TestPage"})}
	   :uri {:encode (fn [s _] s)}
	   :_makeFrame make-frame})

true
