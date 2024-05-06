open Cppish_ast
open Cppish_compile
open Cish_eval



let eval (p: Cppish_ast.program): value = 
  let converted_cish_ast = Cppish_compile.compile_cppish p in
  if Cppish_compile.print_cish_ast then Printf.printf "=== Generated Cish AST ===\n %s\n==========================\n" (Cish_ast.prog2string converted_cish_ast);
  Cish_eval.eval converted_cish_ast
