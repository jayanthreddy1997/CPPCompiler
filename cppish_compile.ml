open Cppish_ast
open Cish_ast

exception NotImplemented

let rec compile_cppish (p: Cppish_ast.program) : Cish_ast.program = 
  match p with 
  | [] -> []
  | fk::rem -> (
    match fk with
    | Fn f -> raise NotImplemented
    | Klass k -> raise NotImplemented
  ) :: (compile_cppish rem)
