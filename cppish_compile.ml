open Cppish_ast
open Cish_ast
exception NotImplemented
exception ClassNotFoundException
exception CompilerError

type string_list_map = (string, string list) Hashtbl.t
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)

(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() = "t" ^ (string_of_int (new_int()))

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

type string_map = (string, string) Hashtbl.t
let object_class_map: string_map = Hashtbl.create 10

let add (map : string_map) (key : string) (value : string) : unit =
  Hashtbl.add map key value

let get (map : string_map) (key : string) : string option =
  Hashtbl.find_opt map key

let (@@) (s1:Cish_ast.stmt) (s2:Cish_ast.stmt) : Cish_ast.stmt = (Cish_ast.Seq (s1, s2), 0)
let su (x: Cish_ast.rstmt): Cish_ast.stmt = (x, 0)
let eu (x: Cish_ast.rexp): Cish_ast.exp = (x, 0)

let rec compile_obj_creation (cname: Cppish_ast.var) (exp_list: Cppish_ast.exp list): Cish_ast.stmt =
  (* malloc the space necessary for the object*)
  (* initialize refcount to 1 using the pointer returned by malloc *)
  (* compile function call for constructor with malloced pointer as the first arg *)
  let new_temp_var = new_temp() in
  let cish_exp_list = List.map compile_exp exp_list in
  match Hashtbl.find_opt class_variable_map cname with
  | None -> raise ClassNotFoundException
  | Some vlist ->
    let malloc_size = (List.length vlist) + 1 in
    su(Cish_ast.Exp(
      eu(Cish_ast.Assign(
          new_temp_var, 
          eu(Cish_ast.Malloc(eu(Cish_ast.Int(malloc_size*4))))
        )
      )
    )) @@
    su(Cish_ast.Exp (
      eu(Cish_ast.Store(
        eu(Var(new_temp_var)), eu(Int(1))
      ))
      )
    ) @@ 
    su (Cish_ast.Exp (
      eu(Cish_ast.Call(
        eu(Cish_ast.Var(cname ^ "_"  ^ cname)), 
        eu(Var(new_temp_var))::cish_exp_list
      ))
    ))

(* Convert the function from Cppish_exp to Cish_exp *)
and compile_exp ((cpp_exp, pos) : Cppish_ast.exp) : Cish_ast.exp =
  let cish_rexp =
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
    | Cppish_ast.Ptr (cname, v, e) -> fst (compile_exp e)
    | Cppish_ast.UniquePtr (cname, v, e) -> raise NotImplemented
    | Cppish_ast.SharedPtr (cname, v, e) -> raise NotImplemented
    | Cppish_ast.Nil -> raise NotImplemented
    | Cppish_ast.New (cname, exp_list) -> raise CompilerError
    | Cppish_ast.Invoke (e, v, exp_list) -> raise NotImplemented
    | Cppish_ast.AttrAccess (e, v) -> 
      let offset = 0 in
      (*
      Case 1: in class def
        we need to know which class we are in. Then we can pass classname as arg to all compile functions
        now, use the class_variables_map for offset
      Case 2: not in class def 
        the classname arg will be effectively null (dummy). in this case, extract the classname from obj_class_map
        and then do the same to compute offset, i.e., refer to class_variables_map for offset.
      *)
      Cish_ast.Load(eu(Cish_ast.Binop((compile_exp e), Cish_ast.Plus, eu(Int(offset)))))
    | Cppish_ast.AttrUpdate (e1, v, e2) -> raise NotImplemented
  in
  (cish_rexp, pos)

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
          let cish_s = compile_stmt s in
            (match e with 
            | (Cppish_ast.New (cname, exp_list), _) -> 
              add object_class_map v cname;
              fst(compile_obj_creation cname exp_list)
            | _ -> 
              let cish_e = compile_exp e in
              Cish_ast.Let (v, cish_e, cish_s) 
            )
    in
    (cish_stmt, pos)

let rec compile_class_function (classname: var) (cpp_func : Cppish_ast.func) : Cish_ast.func =

  match cpp_func with
  | Cppish_ast.Fn cpp_funcsig ->
      let new_func_name = classname ^ "_" ^ cpp_funcsig.name in
      let new_args = "this" :: cpp_funcsig.args in
      let cpp_body = cpp_funcsig.body in
      let cpp_pos = cpp_funcsig.pos in
      let cish_body = compile_stmt cpp_body in
      let cish_funcsig = { 
        Cish_ast.name = new_func_name;
        Cish_ast.args = new_args;
        Cish_ast.body = cish_body;
        Cish_ast.pos = cpp_pos 
      } in
    Cish_ast.Fn cish_funcsig
    

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
          | Cppish_ast.Fn cpp_funcsig -> compile_class_function classname m
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
