(* The abstract syntax for our little subset of C *)
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
type func2sig = { name : var; args : var list; body : stmt; }
type func2 = Fn2 of func2sig

type classsig = {
  cname: class_name;
  cbody: func2 list
}

type klass = Class of classsig

let skip : rstmt = Exp(Int 0,0)          (* simulate a skip statement *)

type func_klass = 
  | Fn of funcsig
  | Class of classsig

type program = func_klass list

