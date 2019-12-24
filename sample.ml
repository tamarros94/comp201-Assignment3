#use "tag-parser.ml";;
open Tag_Parser;;
#use "semantic-analyser.ml";;
open Semantics;;

(* tag_parse_expression (
    Pair(Symbol "lambda", Pair(Pair(Symbol "a", Pair(Symbol "b", Nil)), Pair(Pair(Symbol "+", Pair(Pair(Symbol "*", Pair(Symbol "a", Pair(Symbol "a", Nil))), Pair(Pair(Symbol "*", Pair(Symbol "b", Pair(Symbol "b", Nil))), Nil))), Nil))) 
);; *)

annotate_lexical_addresses(
    LambdaSimple (["a"; "b"],
 Applic (Var "+",
  [Applic (Var "*", [Var "a"; Var "a"]); Applic (Var "*", [Var "b"; Var "b"])]))
);;