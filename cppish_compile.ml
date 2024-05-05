open Cppish_ast
open Cish_ast
exception NotImplemented

(* Convert the function from Cppish_exp to Cish_exp *)
let rec compile_exp ((cpp_exp, pos) : Cppish_ast.exp) : Cish_ast.exp =
  let cish_exp =
    match cpp_exp with
    | Cppish_ast.Int i -> Cish_ast.Int i
    | Cppish_ast.Var v -> Cish_ast.Var v
    | Cppish_ast.Binop (e1, op, e2) ->
        let cish_e1 = compile_exp e1 in
        let cish_e2 = compile_exp e2 in
        let cish_op = 
          match op with
          | Cppish_ast.Plus -> Cish_ast.Plus
          | Cppish_ast.Minus -> Cish_ast.Minus
          | Cppish_ast.Times -> Cish_ast.Times
          | Cppish_ast.Div -> Cish_ast.Div
          | Cppish_ast.Eq -> Cish_ast.Eq
          | Cppish_ast.Neq -> Cish_ast.Neq
          | Cppish_ast.Lt -> Cish_ast.Lt
          | Cppish_ast.Lte -> Cish_ast.Lte
          | Cppish_ast.Gt -> Cish_ast.Gt
          | Cppish_ast.Gte -> Cish_ast.Gte
        in
        Cish_ast.Binop (cish_e1, cish_op, cish_e2)
    | Cppish_ast.Not e ->
        let cish_e = compile_exp e in
        Cish_ast.Not cish_e
    | Cppish_ast.And (e1, e2) ->
        let cish_e1 = compile_exp e1 in
        let cish_e2 = compile_exp e2 in
        Cish_ast.And (cish_e1, cish_e2)
    | Cppish_ast.Or (e1, e2) ->
        let cish_e1 = compile_exp e1 in
        let cish_e2 = compile_exp e2 in
        Cish_ast.Or (cish_e1, cish_e2)
    | Cppish_ast.Assign (v, e) ->
        let cish_e = compile_exp e in
        Cish_ast.Assign (v, cish_e)
    | Cppish_ast.Call (f, args) ->
        let cish_f = (Cish_ast.Var f, pos) in
        let cish_args = List.map compile_exp args in
        Cish_ast.Call (cish_f, cish_args)
    | Cppish_ast.Load e ->
        let cish_e = compile_exp e in
        Cish_ast.Load cish_e
    | Cppish_ast.Store (e1, e2) ->
        let cish_e1 = compile_exp e1 in
        let cish_e2 = compile_exp e2 in
        Cish_ast.Store (cish_e1, cish_e2)
    | Cppish_ast.Malloc e ->
        let cish_e = compile_exp e in
        Cish_ast.Malloc cish_e
    (* TODO:  Compilation of Cppish Pointers to Cish Pointers *)
    | Cppish_ast.Ptr (cname, v, e) -> raise NotImplemented
    | Cppish_ast.UniquePtr (cname, v, e) -> raise NotImplemented
    | Cppish_ast.SharedPtr (cname, v, e) -> raise NotImplemented
    | Cppish_ast.Nil -> raise NotImplemented
    | Cppish_ast.New (cname, exp_list) -> raise NotImplemented
    | Cppish_ast.Invoke (e, v, exp_list) -> raise NotImplemented
  in
  (cish_exp, pos)

  let rec compile_stmt ((cpp_stmt, pos) : Cppish_ast.stmt) : Cish_ast.stmt =
    let cish_stmt =
      match cpp_stmt with
      | Cppish_ast.Exp e ->
          let cish_e = compile_exp e in
          Cish_ast.Exp cish_e
      | Cppish_ast.Seq (s1, s2) ->
          let cish_s1 = compile_stmt s1 in
          let cish_s2 = compile_stmt s2 in
          Cish_ast.Seq (cish_s1, cish_s2)
      | Cppish_ast.If (e, s1, s2) ->
          let cish_e = compile_exp e in
          let cish_s1 = compile_stmt s1 in
          let cish_s2 = compile_stmt s2 in
          Cish_ast.If (cish_e, cish_s1, cish_s2)
      | Cppish_ast.While (e, s) ->
          let cish_e = compile_exp e in
          let cish_s = compile_stmt s in
          Cish_ast.While (cish_e, cish_s)
      | Cppish_ast.For (e1, e2, e3, s) ->
          let cish_e1 = compile_exp e1 in
          let cish_e2 = compile_exp e2 in
          let cish_e3 = compile_exp e3 in
          let cish_s = compile_stmt s in
          Cish_ast.For (cish_e1, cish_e2, cish_e3, cish_s)
      | Cppish_ast.Return e ->
          let cish_e = compile_exp e in
          Cish_ast.Return cish_e
      | Cppish_ast.Let (v, e, s) ->
          let cish_e = compile_exp e in
          let cish_s = compile_stmt s in
          Cish_ast.Let (v, cish_e, cish_s)
    in
    (cish_stmt, pos)

(* Convert the function from Cppish_ast to Cish_ast *)
let rec compile_function (cpp_func : Cppish_ast.func) : Cish_ast.func =
  match cpp_func with
  | Cppish_ast.Fn cpp_funcsig ->
      let cpp_name = cpp_funcsig.name in
      let cpp_args = cpp_funcsig.args in
      let cpp_body = cpp_funcsig.body in
      let cpp_pos = cpp_funcsig.pos in
      let cish_body = compile_stmt cpp_body in
      let cish_funcsig = { 
        Cish_ast.name = cpp_name;
        Cish_ast.args = cpp_args;
        Cish_ast.body = cish_body;
        Cish_ast.pos = cpp_pos 
      } in
    Cish_ast.Fn cish_funcsig

let rec compile_cppish (p: Cppish_ast.program) : Cish_ast.program = 
  match p with 
  | [] -> []
  | fk::rem -> (
    match fk with
    | Fn f -> compile_function f
    | Klass k -> raise NotImplemented
  ) :: (compile_cppish rem)
