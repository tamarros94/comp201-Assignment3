#use "tag-parser.ml";;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of expr' * expr'
  | Def' of expr' * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq var1 var2) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;

(* module type SEMANTICS =  *)
(* sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;; *)

module Semantics 
(* : SEMANTICS  *)
= struct
      
let rec annotate_bound_major string_var arg_lists major_level = 
  let rec annotate_bound_minor string_var arg_list minor_level = match arg_list with
  | car :: cdr -> if String.equal car string_var then Some(minor_level) else annotate_bound_minor string_var cdr (minor_level + 1)
  | _ -> None in

match arg_lists with
| [arg_list :: cdr] -> let minor = annotate_bound_minor string_var arg_list 0 in (match minor with 
   | Some(minor_level) -> if (major_level < 0) then Var'(VarParam(string_var, minor_level)) else Var'(VarBound(string_var, major_level, minor_level))
   | None -> annotate_bound_major string_var [cdr] (major_level + 1))
| _ -> Var'(VarFree(string_var))
 ;;

let rec annotate_lexical_rec e = match e with
  | Const(constant) -> Const'(constant)
  | Var(string) -> Var'(VarFree(string))
  | If(test, dit, dif) -> If'(annotate_lexical_rec test, annotate_lexical_rec dit, annotate_lexical_rec dif)
  | Seq(expr_list) -> Seq'(List.map annotate_lexical_rec expr_list)
  | Set(expr_var, expr_val) -> Set'(annotate_lexical_rec expr_var,annotate_lexical_rec expr_val)
  | Def(expr_var, expr_val) -> Def'(annotate_lexical_rec expr_var,annotate_lexical_rec expr_val)
  | Or(expr_list) -> Or'(List.map annotate_lexical_rec expr_list)
  | Applic(expr, expr_list) -> Applic'(annotate_lexical_rec expr,List.map annotate_lexical_rec expr_list)
  | LambdaSimple(arg_list, body) -> let annotated_body = (annotate_lambda_simple [arg_list] body) in LambdaSimple'(arg_list,annotated_body)
  | LambdaOpt(arg_list, opt_arg, body) -> let annotated_body = annotate_lambda_simple [arg_list@[opt_arg]] body in 
  LambdaOpt'(arg_list, opt_arg, annotated_body)

  and annotate_lambda_simple arg_lists body = match body with
  | Const(constant) -> Const'(constant)
  | Var(str) -> annotate_bound_major str [arg_lists] (-1)
  | If(test, dit, dif) -> If'(annotate_lambda_simple arg_lists test, annotate_lambda_simple arg_lists dit, annotate_lambda_simple arg_lists dif)
  | Seq(expr_list) -> Seq'(List.map (annotate_lambda_simple arg_lists) expr_list)
  | Set(expr_var, expr_val) -> Set'(annotate_lambda_simple arg_lists expr_var,annotate_lambda_simple arg_lists expr_val)
  | Def(expr_var, expr_val) -> Def'(annotate_lambda_simple arg_lists expr_var,annotate_lambda_simple arg_lists expr_val)
  | Or(expr_list) -> Or'(List.map (annotate_lambda_simple arg_lists) expr_list)
  | Applic(expr, expr_list) -> Applic'(annotate_lambda_simple arg_lists expr,List.map (annotate_lambda_simple arg_lists) expr_list)
  | LambdaSimple(arg_list, inner_body) -> let annotated_body = annotate_lambda_simple ([arg_list]@arg_lists) inner_body in LambdaSimple'(arg_list,annotated_body)
  | LambdaOpt(arg_list, opt_arg, inner_body) -> let annotated_body = annotate_lambda_simple ([arg_list @ [opt_arg]]@arg_lists) inner_body 
  in  LambdaOpt'(arg_list, opt_arg, annotated_body)
  ;;

let rec annotate_tail_rec in_tp e = match e with
  | If'(test, dit, dif) -> If'(annotate_tail_rec false test, annotate_tail_rec in_tp dit, annotate_tail_rec in_tp dif)
  | Seq'(expr_list) -> annotate_seq in_tp expr_list
  | Set'(expr_var, expr_val) -> Set'(annotate_tail_rec false expr_var,annotate_tail_rec false expr_val)
  | Def'(expr_var, expr_val) -> Def'(annotate_tail_rec false expr_var,annotate_tail_rec false expr_val)
  | Or'(expr_list) -> annotate_or in_tp expr_list
  | Applic'(expr, expr_list) -> if in_tp then ApplicTP'(annotate_tail_rec false expr,List.map (annotate_tail_rec false) expr_list) else Applic'(annotate_tail_rec false expr,List.map (annotate_tail_rec false) expr_list)
  | LambdaSimple'(arg_list, body) -> LambdaSimple'(arg_list, annotate_tail_rec true body)
  | LambdaOpt'(arg_list, opt_arg, body) -> LambdaOpt'(arg_list, opt_arg, annotate_tail_rec true body)
  | other -> other

and annotate_seq in_tp expr_list = (match (List.rev expr_list) with 
    | car :: cdr -> Seq'((List.map (annotate_tail_rec false) (List.rev cdr)) @ [(annotate_tail_rec in_tp car)])
    | _ -> Seq'((List.map (annotate_tail_rec false) expr_list)))

and annotate_or in_tp expr_list = (match (List.rev expr_list) with 
    | car :: cdr -> Or'((List.map (annotate_tail_rec false) (List.rev cdr)) @ [(annotate_tail_rec in_tp car)])
    | _ -> Or'((List.map (annotate_tail_rec false) expr_list)))


let rec box_set_arg arg body = Const'(Void)
(* match body with
| Var'(VarParam(str)) -> if String.equal str arg then [true; false; false] else [false; false; false]
| Var'(VarBound(str, major, minor)) -> if String.equal str arg then [true; false; false] else [false; false; false]
| Set'(Var'(VarParam(str1)), Var'(VarParam(str2))) -> if String.equal str1 arg
then if String.equal str2 arg then [true; true; false] else [false; true; false]
else if String.equal str2 arg then [true; false; false] else [false; false; false]
| Set'(Var'(VarParam(str)), not_var) -> if String.equal str arg then [false; true; false] else [false; false; false]
| LambdaSimple'(arg_list, body) -> match box_set_arg arg body with
  | r::w::f -> [r; w; true] *)
;;

let rec box_set_lambda arg_list body = match arg_list with
  | car :: cdr -> let new_body = box_set_arg car body  in box_set_lambda cdr new_body
  | [] -> body ;;

let rec box_set_rec e = match e with
  | If'(test, dit, dif) -> If'(box_set_rec test, box_set_rec dit, box_set_rec dif)
  | Seq'(expr_list) -> Seq'(List.map box_set_rec expr_list)
  | Set'(expr_var, expr_val) -> Set'(box_set_rec expr_var,box_set_rec expr_val)
  | Def'(expr_var, expr_val) -> Def'(box_set_rec expr_var,box_set_rec expr_val)
  | Or'(expr_list) -> Or'(List.map box_set_rec expr_list)
  | Applic'(expr, expr_list) -> Applic'(box_set_rec expr, List.map box_set_rec expr_list) 
  | ApplicTP'(expr, expr_list) -> ApplicTP'(box_set_rec expr, List.map box_set_rec expr_list) 
  | LambdaSimple'(arg_list, body) -> box_set_lambda arg_list body
  | LambdaOpt'(arg_list, opt_arg, body) -> box_set_lambda [arg_list@[opt_arg]] body
  | other -> other;;

let annotate_lexical_addresses e = annotate_lexical_rec e;;

let annotate_tail_calls e = annotate_tail_rec false e;;

let box_set e = box_set_rec e;;

(* let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));; *)
  
end;; (* struct Semantics *)
