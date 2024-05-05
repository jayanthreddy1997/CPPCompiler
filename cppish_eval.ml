open Cppish_ast
open Cppish_compile
open Cish_eval

let eval (p: Cppish_ast.program): value = 
  let converted_cish_ast = Cppish_compile.compile_cppish p in
  Cish_eval.eval converted_cish_ast
