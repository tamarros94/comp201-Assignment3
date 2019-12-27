#use "tag-parser.ml";;
open Tag_Parser;;
#use "semantic-analyser.ml";;
open Semantics;;

(* tag_parse_expression(
Pair(Symbol "lambda", Pair(Pair(Symbol "x", Nil), Pair(Pair(Symbol "lambda", Pair(Nil, Pair(Pair(Symbol "lambda", Pair(Nil, Pair(Symbol "x", Nil))), Pair(Pair(Symbol "lambda", Pair(Pair(Symbol "y", Nil), Pair(Pair(Symbol "set!", Pair(Symbol "x", Pair(Symbol "y", Nil))), Nil))), Nil)))), Nil)))
);; *)
(* annotate_tail_calls 
(
annotate_lexical_addresses (
Def (Var "foo1",
 LambdaSimple (["x"],
  Applic (Var "list",
   [LambdaSimple ([], Var "x"); LambdaSimple (["y"], Set (Var "x", Var "y"))])))
));; *)
let rec append_list l1 l2 =
    match l1 with
    | [] -> l2
    | h::t -> if List.mem h l2 then append_list t l2
              else append_list t (h::l2)

let expr_list = [2;3;4];;
let return_from_rec element = [[element];[1]];;
let test = List.map return_from_rec expr_list;;
let append_inner_lists l1 l2 = match l1, l2 with
| left1::[right1], left2::[right2] -> [append_list left1 left2;append_list right1 right2]
|(l1, []) -> l1
|([], l2) -> l2
| other -> []

let test2 = List.fold_right fold_fun test [] ;;
(* let test3 = fold_fun [[2]; [1]] [[3]; [1]] *)

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