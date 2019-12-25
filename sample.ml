#use "tag-parser.ml";;
open Tag_Parser;;
#use "semantic-analyser.ml";;
open Semantics;;

tag_parse_expression(
Pair(Symbol "lambda", Pair(Pair(Symbol "x", Nil), Pair(Pair(Symbol "lambda", Pair(Nil, Pair(Pair(Symbol "lambda", Pair(Nil, Pair(Symbol "x", Nil))), Pair(Pair(Symbol "lambda", Pair(Pair(Symbol "y", Nil), Pair(Pair(Symbol "set!", Pair(Symbol "x", Pair(Symbol "y", Nil))), Nil))), Nil)))), Nil)))
);;
(* annotate_tail_calls 
(
annotate_lexical_addresses (
Def (Var "foo1",
 LambdaSimple (["x"],
  Applic (Var "list",
   [LambdaSimple ([], Var "x"); LambdaSimple (["y"], Set (Var "x", Var "y"))])))
));; *)

(*
(define sexpr->ocaml-string
(lambda (e)
(cond
((boolean? e)
(if e "Bool true" "Bool false"))
((null? e) "Nil")
((char? e) (format "Char '~a'" e))
((symbol? e) (format "Symbol \"~a\"" e))
((string? e) (format "String \"~a\"" e) )
 ((integer? e) (format "Number (Int ~a)" e))
 ((pair? e)
 (format "Pair(~a, ~a)"
 (sexpr->ocaml-string (car e))
 (sexpr->ocaml-string (cdr e))))
 ((vector? e)
 (format "Vector [~a]"
 (fold-right (lambda (v lst) `(,v ";" ,@lst)) '()
 (map sexpr->ocaml-string (vector->list e)))))
 (else (error 'sexpr->ocaml-string
 (format "Unsupported type: ~a" e))))))

 (define print-template
 (lambda (sexpr)
 (display (sexpr->ocaml-string sexpr))))

 (print-template '
 )


*)