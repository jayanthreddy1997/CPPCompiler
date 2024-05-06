open Cppish_ast
open Cppish_eval

(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file() =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Cppish_parse.program Cppish_lex.lexer (Lexing.from_channel ch)

let parse_stdin() = 
  Cppish_parse.program Cppish_lex.lexer (Lexing.from_channel stdin)

(* Reads in cish code and evaluates it *)
let _ =
  let prog = parse_stdin() in
  let s = Cppish_ast.string_of_program(prog) in
  print_endline (s);
  let ans = eval prog in
  print_string ("answer = "^(Cish_eval.val2string ans)^"\n")
