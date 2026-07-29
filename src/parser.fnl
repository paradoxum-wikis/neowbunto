;; Expression parser

(local {: lex} (require :lexer))

(var parse-additive nil)

(fn at [tokens pos]
	(. tokens pos))

(fn tok-type [tok]
	(and tok tok.type))

(fn tok-op? [tokens pos op]
	(let [tok (at tokens pos)]
		(and tok (= tok.type :op) (= tok.value op))))

(fn tok-binop? [tokens pos ops]
	(let [tok (at tokens pos)]
		(and tok (= tok.type :op) (ops:find tok.value 1 true))))

(fn unexpected [tok]
	(if (not tok)
			"parser: unexpected end of input"
			(.. "parser: unexpected token type=" (tostring tok.type)
					(if tok.value (.. " value=" (tostring tok.value)) ""))))

(fn literal-raw? [raw]
	;; pure <ref>... or pure [[...]]
	;; not formulas that embed them
	(let [s (pick-values 1 (: (tostring (or raw "")) :match "^%s*(.-)%s*$"))]
		(or (s:match "^<ref[%s/>]")
				(s:match "^%[%[[^%[%]]+%]%]$"))))

(fn mw-expr-body? [raw]
	(let [str (tostring (or raw ""))
				s (pick-values 1 (str:match "^%s*(.-)%s*$"))]
		(not= (s:match "^{{#expr:(.*)}}%s*$") nil)))

(fn extract-mw-expr [raw]
	(let [str (tostring (or raw ""))
				s (pick-values 1 (str:match "^%s*(.-)%s*$"))]
		(s:match "^{{#expr:(.*)}}%s*$")))

(fn wikitext-body? [raw]
	(and (raw:find "{{" 1 true)
			 (not (mw-expr-body? raw))))

(fn config-varref? [name]
	(or (name:match "^FNC%-")
			(name:match "^FSE%-")))

(fn cycle-message [stack name]
	(let [parts []]
		(var started false)
		(each [_ n (ipairs stack)]
			(when (or started (= n name))
				(set started true)
				(table.insert parts n)))
		(table.insert parts name)
		(.. "cyclic $VAR$ reference: " (table.concat parts " -> "))))

(fn stack-push [stack name]
	(table.insert stack name))

(fn stack-pop [stack]
	(table.remove stack))

(fn stack-has? [stack name]
	(var found false)
	(each [_ n (ipairs stack) &until found]
		(when (= n name)
			(set found true)))
	found)

(fn parse-pin-parts [name]
	(let [(base lvl br) (name:match "^(.-)@(%d+)@(.+)$")]
		(if base
				(values base (tonumber lvl) br)
				(let [(base2 lvl2) (name:match "^(.-)@(%d+)$")]
					(if base2
							(values base2 (tonumber lvl2) nil)
							(values name nil nil))))))

(var expand-varref nil)
(var parse-formula nil)
(var parse-var nil)

(fn special-varref? [base pin-lvl]
	(or pin-lvl
			(= base "FNC-TOTALPRICE")
			(base:match "^FNC%-TOTAL%-(.+)$")
			(config-varref? base)))

(fn expand-user-macros [raw var-env stack]
	(if (not var-env)
			(tostring (or raw ""))
			(pick-values 1
				(: (tostring (or raw "")) :gsub "%$([^%$]+)%$"
					 (fn [name]
						 (let [(base pin-lvl _) (parse-pin-parts name)]
							 (if (special-varref? base pin-lvl)
									 (.. "$" name "$")
									 (let [body (. var-env base)]
										 (when (= body nil)
											 (error (.. "undefined variable: $" base "$")))
										 (when (stack-has? stack base)
											 (error (cycle-message stack base)))
										 (stack-push stack base)
										 (let [out (expand-user-macros body var-env stack)]
											 (stack-pop stack)
											 out)))))))))

(set parse-var
		 (fn [name var-env parse-cache parsing-stack]
			 (case (. parse-cache name)
				 nil
				 (let [(base _ _) (parse-pin-parts name)]
					 ;; $FNC-TOTALPRICE$ / $FNC-TOTAL-X$ are intrinsics (not formula-env entries)
					 (if (or (= base "FNC-TOTALPRICE")
									 (base:match "^FNC%-TOTAL%-(.+)$"))
							 (let [ast (expand-varref name var-env parse-cache parsing-stack)]
								 (set (. parse-cache name) ast)
								 ast)
							 (do
								 (when (stack-has? parsing-stack name)
									 (error (cycle-message parsing-stack name)))
								 (let [raw (. var-env name)]
									 (when (= raw nil)
										 (error (.. "undefined variable: $" name "$")))
									 (stack-push parsing-stack name)
									 (let [ast (parse-formula raw var-env parse-cache
																						parsing-stack)]
										 (stack-pop parsing-stack)
										 (set (. parse-cache name) ast)
										 ast)))))
				 ast ast)))

(set expand-varref
		 ;; after paste only pins / FNC-* / config keys still need this path
		 (fn [name var-env parse-cache parsing-stack]
			 (let [(base pin-lvl pin-br) (parse-pin-parts name)
						 inner (if (= base "FNC-TOTALPRICE")
											 [:intrinsic :totalprice]
											 (let [total (base:match "^FNC%-TOTAL%-(.+)$")]
												 (if total
														 [:intrinsic :total total]
														 (config-varref? base)
														 [:varref base]
														 (parse-var base var-env parse-cache parsing-stack))))]
				 (if pin-lvl
						 [:pin pin-lvl pin-br inner]
						 inner))))

;; each parse-* returns (ast next-pos)
;; tokens are never sliced
(fn parse-primary [tokens pos var-env parse-cache parsing-stack]
	(let [head (at tokens pos)]
		(if (not head)
				(error "parser: expected expression, got end of input")
				(= head.type :num)
				(values [:num head.value] (+ pos 1))
				(= head.type :ident)
				(values [:ident head.value] (+ pos 1))
				(= head.type :dotref)
				(values [:dotref head.table head.col] (+ pos 1))
				(= head.type :varref)
				(if var-env
						(values (expand-varref head.value var-env parse-cache parsing-stack)
										(+ pos 1))
						(values [:varref head.value] (+ pos 1)))
				(= head.type :lparen)
				(let [(inner p2) (parse-additive tokens (+ pos 1) var-env parse-cache
																				 parsing-stack)]
					(if (not= (tok-type (at tokens p2)) :rparen)
							(error "parser: missing closing ')'")
							(values inner (+ p2 1))))
				(error (unexpected head)))))

;; x++ / x-- -> x ± 1 (HelpNeowtext Increment/Decrement)
(fn parse-postfix [tokens pos var-env parse-cache parsing-stack]
	(var (ast p) (parse-primary tokens pos var-env parse-cache parsing-stack))
	(while (or (tok-op? tokens p "++") (tok-op? tokens p "--"))
		(let [op (. (at tokens p) :value)]
			(set ast (if (= op "++")
									 [:binop "+" ast [:num 1]]
									 [:binop "-" ast [:num 1]]))
			(set p (+ p 1))))
	(values ast p))

(fn parse-unary [tokens pos var-env parse-cache parsing-stack]
	(if (tok-op? tokens pos "-")
			(let [(inner p2) (parse-unary tokens (+ pos 1) var-env parse-cache
																		parsing-stack)]
				(values [:unop "-" inner] p2))
			(parse-postfix tokens pos var-env parse-cache parsing-stack)))

(fn parse-pow [tokens pos var-env parse-cache parsing-stack]
	(let [(lhs p1) (parse-unary tokens pos var-env parse-cache parsing-stack)]
		(if (tok-op? tokens p1 "**")
				(let [(rhs p2) (parse-pow tokens (+ p1 1) var-env parse-cache
																	parsing-stack)]
					(values [:pow lhs rhs] p2))
				(values lhs p1))))

(fn parse-multiplicative [tokens pos var-env parse-cache parsing-stack]
	(var (lhs p) (parse-pow tokens pos var-env parse-cache parsing-stack))
	(while (tok-binop? tokens p "*/%")
		(let [op (. (at tokens p) :value)
					(rhs p2) (parse-pow tokens (+ p 1) var-env parse-cache parsing-stack)]
			(set lhs [:binop op lhs rhs])
			(set p p2)))
	(values lhs p))

(set parse-additive
		 (fn [tokens pos var-env parse-cache parsing-stack]
			 (var (lhs p) (parse-multiplicative tokens pos var-env parse-cache
																					parsing-stack))
			 (while (tok-binop? tokens p "+-")
				 (let [op (. (at tokens p) :value)
							 (rhs p2) (parse-multiplicative tokens (+ p 1) var-env parse-cache
																							parsing-stack)]
					 (set lhs [:binop op lhs rhs])
					 (set p p2)))
			 (values lhs p)))

(fn parse [tokens]
	(when (or (not tokens) (= (length tokens) 0))
		(error "parser: empty token list"))
	(let [(ast pos) (parse-additive tokens 1 nil nil nil)]
		(when (<= pos (length tokens))
			(error (unexpected (at tokens pos))))
		ast))

(fn parse-string [s]
	(parse (lex s)))

(set parse-formula
		 (fn [raw var-env parse-cache parsing-stack]
			 (if (literal-raw? raw)
					 [:literal raw]
					 (let [expr-body (extract-mw-expr raw)]
						 (if expr-body
								 [:mw-expr expr-body]
								 (wikitext-body? raw)
								 [:wikitext raw]
								 (let [stack (or parsing-stack [])
											 expanded (expand-user-macros raw var-env stack)]
									 ;; paste can turn the whole formula into pure <ref> / [[...]] / {{...}}
									 (if (literal-raw? expanded)
											 [:literal expanded]
											 (let [eb (extract-mw-expr expanded)]
												 (if eb
														 [:mw-expr eb]
														 (wikitext-body? expanded)
														 [:wikitext expanded]
														 (let [tokens (lex expanded)]
															 (when (= (length tokens) 0)
																 (error "parser: empty formula"))
															 (let [(ast pos) (parse-additive tokens 1
																															var-env
																															parse-cache
																															stack)]
																 (when (<= pos (length tokens))
																	 (error (unexpected (at tokens pos))))
																 ast)))))))))))


(fn parse-with-env [s var-env]
	(let [parse-cache {}
				parsing-stack []
				ast (parse-formula s var-env parse-cache parsing-stack)]
		(values ast parse-cache)))

(fn parse-var-env [var-env]
	(let [parse-cache {}
				parsing-stack []]
		(each [name _ (pairs var-env)]
			(when (and (not (config-varref? name))
								 (= (. parse-cache name) nil))
				(parse-var name var-env parse-cache parsing-stack)))
		parse-cache))

{:parse parse
 :parse-string parse-string
 :parse-formula parse-formula
 :parse-var parse-var
 :parse-with-env parse-with-env
 :parse-var-env parse-var-env
 :parse-pin-parts parse-pin-parts
 :literal-raw? literal-raw?
 :mw-expr-body? mw-expr-body?
 :extract-mw-expr extract-mw-expr
 :wikitext-body? wikitext-body?
 :config-varref? config-varref?
 :parse-additive parse-additive}
