(* The abstract syntax for our little subset of C++ *)
type var = string
type pos = int      (* position is line number in source file *)
type class_name = string

type binop = 
  Plus | Minus | Times | Div          (* +, -, *, /           *)
| Eq | Neq | Lt | Lte | Gt | Gte      (* ==, !=, <, <=, >, >= *)

type rexp = 
  Int of int
| Var of var
| Ptr of class_name * var * exp             (* Calculator *x *)
| UniquePtr of class_name * var * exp       (* unique_ptr<ClassName> ptrName *)
| SharedPtr of class_name * var * exp       (* shared_ptr<ClassName> ptrName *)
| Binop of exp * binop * exp
| Not of exp                          (* !x *)
| And of exp * exp                    (* x < y && y < z *)
| Or of exp * exp                     (* x < y || x < z *)
| Assign of var * exp                 (* x = y+42 *)
| Call of var * (exp list)            (* f(x,y,z) *)
| Nil
| New of class_name * (exp list)      (* new Calculator(arg1, arg2) *)
| Invoke of exp * var * (exp list)    (* obj.method_name(arg1, arg2) and *)
| Load of exp                         (* *(x+3) *)
| Store of exp * exp                  (* *(x+3) = e *)
| Malloc of exp                       (* malloc(i) *)
  (* every expression comes with its position *)
and exp = rexp * pos

type rstmt = 
  Exp of exp                          (* x = 3+4; *)
| Seq of stmt * stmt                  (* x = 2*9; y = 42; *)
| If of exp * stmt * stmt             (* if (x == y) x = 42 else y = 43 *)
| While of exp * stmt                 (* while (x < y) x = x + 1; *)
| For of exp * exp * exp * stmt       (* for (x=0; x<y; x=x+1) y=y*42; *)
| Return of exp                       (* return e; *)
| Let of var * exp * stmt             (* let x=3; in x=x+1; *)

  (* every statement comes with its position *)
and stmt = rstmt * pos


type funcsig = { name : var; args : var list; body : stmt; pos : pos }
type func = Fn of funcsig

type classsig = {
  cname: class_name;
  cvars: var list;
  cmethods: func list
}

type class_member = {
  cvars: var list;
  cmethods: func list
}

type klass = Klass of classsig

let skip : rstmt = Exp(Int 0,0)          (* simulate a skip statement *)

type func_klass = 
  | Fn2 of func
  | Klass of klass

type program = func_klass list


(***************************************************************)
(* functions for printing out Cppish syntax                    *)
(***************************************************************)
exception AstPrintException

(* Helper function to convert a binary operator to its string representation *)
let string_of_binop op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="

(* Helper function to convert a list of variables to a comma-separated string *)
let string_of_var_list vars =
  String.concat ", " vars

let rec string_of_newobj ((expr,_) : exp) : string =
  match expr with 
  | Nil -> "nullptr"
  | New (cn, el) -> "new " ^ cn ^ "(" ^ (string_of_explist el) ^ ")"
  | _ -> raise AstPrintException
and string_of_explist (es: exp list): string =
  match es with 
  | [] -> ""
  | e::rem_es -> (string_of_exp e) ^ ", " ^ (string_of_explist rem_es)
  

(* Convert an expression (rexp) to a string *)
and  string_of_exp ((expr,_) : exp) : string =
  match expr with
  | Int n -> string_of_int n
  | Var v -> v
  | Ptr (cname, v, e) ->  cname ^ " [PTR]*" ^ v ^ " = " ^ (string_of_newobj e)
  | UniquePtr (cname, v, e) -> "unique_ptr<" ^ cname ^ "> " ^ v
  | SharedPtr (cname, v, e) -> "shared_ptr<" ^ cname ^ "> " ^ v
  | Binop (e1, op, e2) ->
      string_of_exp e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_exp e2
  | Not e -> "!" ^ string_of_exp e
  | And (e1, e2) -> string_of_exp e1 ^ " && " ^ string_of_exp e2
  | Or (e1, e2) -> string_of_exp e1 ^ " || " ^ string_of_exp e2
  | Assign (v, e) -> v ^ " = " ^ string_of_exp e
  | Call (f, args) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_exp args) ^ ")"
  | Nil -> "nullptr"
  | New (cname, args) ->
      "new " ^ cname ^ "(" ^ String.concat ", " (List.map string_of_exp args) ^ ")"
  | Invoke (obj, method_name, args) ->
    string_of_exp obj ^ "." ^ method_name ^ "(" ^ String.concat ", " (List.map string_of_exp args) ^ ")"
  | Load e -> "*(" ^ string_of_exp e ^ ")"
  | Store (e1, e2) -> "*(" ^ string_of_exp e1 ^ ") = " ^ string_of_exp e2
  | Malloc e -> "malloc(" ^ string_of_exp e ^ ")"

(* Convert a statement (rstmt) to a string *)
and string_of_rstmt (stmt : rstmt) : string =
  match stmt with
  | Exp (e) -> string_of_exp e ^ ";"
  | Seq (s1, s2) -> string_of_rstmt (fst s1) ^ " " ^ string_of_rstmt (fst s2)
  | If (e, s1, s2) ->
      "if (" ^ string_of_exp e ^ ") " ^ string_of_rstmt (fst s1) ^ " else " ^ string_of_rstmt (fst s2)
  | While (e, s) -> "while (" ^ string_of_exp e ^ ") " ^ string_of_rstmt (fst s)
  | For (init, cond, incr, body) ->
      "for (" ^ string_of_exp init ^ "; " ^ string_of_exp cond ^ "; " ^ string_of_exp incr ^ ") " ^
      string_of_rstmt (fst body)
  | Return e -> "return " ^ string_of_exp e ^ ";"
  | Let (v, e, body) -> "let " ^ v ^ " = " ^ string_of_exp e ^ " in " ^ string_of_rstmt (fst body)

(* Convert a function signature (funcsig) to a string *)
let string_of_funcsig (fs : funcsig) : string =
  "func " ^ fs.name ^ "(" ^ string_of_var_list fs.args ^ ") {" ^
  string_of_rstmt (fst fs.body) ^ "}"

(* Convert a function (func) to a string *)
let string_of_func (f : func) : string =
  let Fn(fs) = f in
  string_of_funcsig fs

(* Convert a class signature (classsig) to a string *)
let string_of_classsig (cs : classsig) : string =
  "class " ^ cs.cname ^ " {" ^
  (* Convert class variables *)
  (if List.length cs.cvars > 0 then
    "\n  // Variables: " ^ string_of_var_list cs.cvars ^ "\n"
  else "") ^
  (* Convert class methods *)
  (String.concat "\n\n" (List.map string_of_func cs.cmethods)) ^
  "\n};"

(* Convert a class (klass) to a string *)
let string_of_klass (k : klass) : string =
  match k with
  | Klass cs -> string_of_classsig cs

(* Convert a func_klass (either func or klass) to a string *)
let string_of_func_klass (fk : func_klass) : string =
  match fk with
  | Fn2 f -> string_of_func f
  | Klass k -> string_of_klass k

(* Convert a program (list of func_klass) to a string *)
let string_of_program (p : program) : string =
  String.concat "\n\n" (List.map string_of_func_klass p)