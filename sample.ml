#use "tag-parser.ml";;
open Tag_Parser;;
#use "semantic-analyser.ml";;
open Semantics;;

(* tag_parse_expression(
Pair(Symbol "lambda", Pair(Pair(Symbol "a", Nil), Pair(Pair(Symbol "a", Pair(Pair(Symbol "a", Pair(Pair(Symbol "lambda", Pair(Pair(Symbol "b", Nil), Pair(Pair(Symbol "b", Pair(Pair(Symbol "b", Pair(Pair(Symbol "a", Pair(Symbol "c", Nil)), Nil)), Nil)), Nil))), Nil)), Nil)), Nil))) 
);; *)

annotate_tail_calls 
(annotate_lexical_addresses
(
LambdaSimple (["a"],
 Applic (Var "a",
  [Applic (Var "a",
    [LambdaSimple (["b"],
      Applic (Var "b", [Applic (Var "b", [Applic (Var "a", [Var "c"])])]))])]))
)
);;

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