open Cppish_ast
open Cish_ast
exception NotImplemented

type string_list_map = (string, string list) Hashtbl.t

(* Create a new string list map *)
let class_method_map : string_list_map = Hashtbl.create 10
let class_variable_map: string_list_map = Hashtbl.create 10

let print_string_list_map (map : string_list_map) : unit =
  Hashtbl.iter (fun key values ->
    Printf.printf "%s: [%s]\n" key (String.concat "; " values)
  ) map

(* Add a value to the map *)
let add_to_map (map : string_list_map) (key : string) (value : string) : unit =
  match Hashtbl.find_opt map key with
  | Some ls -> Hashtbl.replace map key (ls @ [value])
  | None -> Hashtbl.add map key [value]

(* Check if a key-value pair exists in the map *)
let is_exist_in_map (map : string_list_map) (key : string) (value : string) : bool =
  match Hashtbl.find_opt map key with
  | Some ls -> List.mem value ls
  | None -> false

(* Convert the function from Cppish_exp to Cish_exp *)
let rec compile_exp ((cpp_exp, pos) : Cppish_ast.exp) : Cish_ast.exp =
  let cish_exp =
    match cpp_exp with
    | Cppish_ast.Int i -> Cish_ast.Int i
    | Cppish_ast.Var v -> Cish_ast.Var v
    | Cppish_ast.Binop (e1, op, e2) ->
        let cish_e1 = compile_exp e1 in
        let cish_e2 = compile_exp e2 in
        let cish_op = (
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
          | Cppish_ast.Gte -> Cish_ast.Gte)
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
    | Cppish_ast.AttrAccess (e, v) -> raise NotImplemented
    | Cppish_ast.AttrUpdate (e1, v, e2) -> raise NotImplemented
  in
  (cish_exp, pos)

  (* Convert the function from Cppish_stmt to Cish_stmt *)
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

let compile_class (klass : Cppish_ast.klass) : Cish_ast.func list =
  match klass with
  | Cppish_ast.Klass cpp_classsig ->
      let classname = cpp_classsig.cname in
      let methods = cpp_classsig.cmethods in
      let variables = cpp_classsig.cvars in

      (* List.iter (fun f -> 
        print_endline "Printing method ";
        let x = string_of_func f in
        print_endline x;
      ) methods; *)
      
      (* Add class methods to the global method map *)
      List.iter (fun func ->
        match func with
        | Cppish_ast.Fn funcsig ->
            add_to_map class_method_map classname funcsig.name
      ) methods;

      (* Add class variables to the global variable map *)
      List.iter (fun var ->
        add_to_map class_variable_map classname var
      ) variables;
      
      (* Convert methods to Cish_ast functions *)
      List.map (fun m ->
        match m with
          | Cppish_ast.Fn cpp_funcsig ->
            let new_func_name = classname ^ "_" ^ cpp_funcsig.name in
            let updated_cpp_funcsig = { cpp_funcsig with name = new_func_name } in
            compile_function (Cppish_ast.Fn updated_cpp_funcsig )
      ) methods

let rec compile_cppish (p: Cppish_ast.program) : Cish_ast.program = 
  (* let _x = string_of_program p in *)
  match p with 
  | [] -> []
  | fk::rem -> (
    match fk with
    | Fn2 f -> compile_function f :: (compile_cppish rem)
    | Klass k -> compile_class k @ (compile_cppish rem)
  )
