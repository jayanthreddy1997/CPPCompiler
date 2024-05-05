%{
open Cppish_ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
%}

%start program

/* nonterminals */
%type <Cppish_ast.program> program
%type <Cppish_ast.class_member> class_member
%type <Cppish_ast.klass> klass
%type <Cppish_ast.func_klass> func_klass
%type <Cppish_ast.func> func
%type <Cppish_ast.var list> idlist
%type <Cppish_ast.var list> formals
%type <Cppish_ast.stmt> stmt
%type <Cppish_ast.stmt> stmtlist
%type <Cppish_ast.exp> yexp
%type <Cppish_ast.exp list> explist
%type <Cppish_ast.exp> orexp
%type <Cppish_ast.exp> andexp
%type <Cppish_ast.exp> equalexp
%type <Cppish_ast.exp> relnexp
%type <Cppish_ast.exp> addexp
%type <Cppish_ast.exp> mulexp
%type <Cppish_ast.exp> unaryexp
%type <Cppish_ast.exp> atomicexp
%type <Cppish_ast.rexp> funccall
%type <Cppish_ast.exp> newobj
%type <Cppish_ast.exp option> expopt

/* terminals */
%token SEMI LPAREN RPAREN LBRACE RBRACE
%token EQEQ NEQ LTE GTE LT GT EQ BANG 
%token PLUS MINUS TIMES DIV AND OR
%token RETURN IF ELSE WHILE FOR
%token LET COMMA CLASS UNIQUE_PTR SHARED_PTR NEW DOT NIL MALLOC
%token <int> INT 
%token <string> ID
%token <string> ID2
%token EOF

%left COMMA
%left OR
%left AND
%left LTE GTE LT GT
%left EQEQ NEQ
%left PLUS MINUS
%left TIMES DIV // TODO: we are missing % operator
%left BANG
%left DOT
%left LPAREN RPAREN

/* Start grammer rules*/
%%

program:
  | func_klass { [$1] }
  | func_klass program { $1 :: $2 }

func_klass:
    klass SEMI { Klass $1 }
  | func { Fn2 $1 }
  

klass:
  CLASS ID LBRACE class_member RBRACE { Klass{cname=$2;cvars=$4.cvars;cmethods=$4.cmethods} }

// TODO: need to define class_member and few others as a nonterminal
class_member:
    LET ID SEMI class_member { {cvars=$2 :: $4.cvars; cmethods=$4.cmethods} }
  | func class_member { {cvars=$2.cvars; cmethods=$1 :: $2.cmethods} }
  | /* empty */ { {cvars=[]; cmethods=[]} }

func :
  ID formals LBRACE stmtlist RBRACE { Fn{name=$1;args=$2;body=$4;pos=rhs 1} }

formals :
  LPAREN RPAREN { [] }
| LPAREN idlist RPAREN { $2 }

idlist :
  ID { [$1] }
| ID COMMA idlist { $1::$3 }

stmt : 
  SEMI { (skip, rhs 1) }
| yexp SEMI { (Exp $1, rhs 1) }
| RETURN yexp SEMI { (Return $2, rhs 1) }
| LBRACE stmtlist RBRACE { $2 }
| IF LPAREN yexp RPAREN stmt ELSE stmt { (If($3,$5,$7), rhs 1) } 
| IF LPAREN yexp RPAREN stmt { (If($3,$5,(skip, rhs 5)), rhs 1) } 
| WHILE LPAREN yexp RPAREN stmt { (While($3,$5), rhs 1) }
| FOR LPAREN expopt SEMI expopt SEMI expopt RPAREN stmt {
      let e1 = match $3 with None -> (Int(0), rhs 3) | Some e -> e in
      let e2 = match $5 with None -> (Int(1), rhs 5) | Some e -> e in
      let e3 = match $7 with None -> (Int(0), rhs 7) | Some e -> e in
      (For(e1,e2,e3,$9), rhs 1)
    }
| LET ID EQ yexp SEMI stmt { (Let($2,$4,$6), rhs 1) }
| LET ID ID EQ newobj SEMI stmt { (Let($3, (Ptr($2, $3, $5), rhs 1), $7), rhs 1) }

stmtlist :
  stmt { $1 }
| stmt stmtlist { (Seq($1,$2), rhs 1) }

expopt : 
  { None }
  | yexp { Some $1 }

yexp:
  orexp { $1 }
| ID EQ yexp { (Assign($1,$3), rhs 1) }
| ID DOT ID EQ yexp { (AttrUpdate((Var($1), rhs 1), $3, $5), rhs 1) }
// | ID TIMES ID EQ newobj{ (Ptr($1, $3, $5), rhs 1) } // class_name *p = new class_name();
| TIMES yexp EQ newobj { (Store($2, $4), rhs 1)}  // *(p + y) = new class_name();
| TIMES addexp EQ yexp{ (Store($2, $4), rhs 1)} //TODO: Revisit this.  // *(p+4) = e
| TIMES addexp { (Load($2), rhs 1)} //TODO: Revisit this.  // *(p+4)
| UNIQUE_PTR LT ID GT ID LPAREN newobj RPAREN { (UniquePtr($3, $5, $7), rhs 1)} 
| SHARED_PTR LT ID GT ID LPAREN ID RPAREN { (SharedPtr($3, $5, (Var($7), rhs 1)), rhs 1)} // p = shared_ptr<class_name>(y)
| SHARED_PTR LT ID GT ID LPAREN newobj RPAREN { (SharedPtr($3, $5, $7), rhs 1)}

newobj:
| NEW ID LPAREN RPAREN {(New($2, []), rhs 1)}
| NEW ID LPAREN explist RPAREN {(New($2, $4), rhs 1)}
| NIL {(Nil, rhs 1)}

explist :
  yexp { [$1] }
| yexp COMMA explist { $1::$3 }

orexp :
  andexp { $1 }
| orexp OR andexp { (Or($1,$3), rhs 1) }

andexp :
  equalexp { $1 }
| andexp AND equalexp { (And($1,$3), rhs 1) }

equalexp :
  relnexp { $1 }
| equalexp EQEQ relnexp { (Binop($1,Eq,$3), rhs 1) }
| equalexp NEQ relnexp { (Binop($1,Neq,$3), rhs 1) }

relnexp :
  addexp { $1 }
| relnexp LT addexp { (Binop($1,Lt,$3), rhs 1) }
| relnexp GT addexp { (Binop($1,Gt,$3), rhs 1) }
| relnexp LTE addexp { (Binop($1,Lte,$3), rhs 1) }
| relnexp GTE addexp { (Binop($1,Gte,$3), rhs 1) }

addexp :
  mulexp { $1 }
| addexp PLUS mulexp { (Binop($1,Plus,$3), rhs 1) }
| addexp MINUS mulexp { (Binop($1,Minus,$3), rhs 1) }

mulexp :
  unaryexp { $1 }
| mulexp TIMES unaryexp { (Binop($1,Times,$3), rhs 1) }
| mulexp DIV unaryexp { (Binop($1,Div,$3), rhs 1) }

unaryexp :
  atomicexp { $1 }
| PLUS unaryexp { $2 }
| MINUS unaryexp { (Binop((Int 0,rhs 1),Minus,$2), rhs 1) }
| BANG unaryexp { (Not $2, rhs 1) }

atomicexp :
  INT { (Int $1, rhs 1) }
| ID { (Var $1, rhs 1) }
| funccall{ ($1, rhs 1) }
| MALLOC LPAREN yexp RPAREN { (Malloc($3), rhs 1) }
| LPAREN yexp RPAREN { $2 }
| ID DOT ID LPAREN RPAREN { (Invoke((Var($1), rhs 1), $3, []), rhs 1) } // TODO: check if yexp is enough
| ID DOT ID LPAREN explist RPAREN { (Invoke((Var($1), rhs 1), $3, $5), rhs 1) }
| ID DOT ID { (AttrAccess((Var($1), rhs 1), $3), rhs 1) }
| LPAREN yexp RPAREN DOT ID LPAREN explist RPAREN { (Invoke($2, $5, $7), rhs 1) }
| LPAREN yexp RPAREN DOT ID { (AttrAccess($2, $5), rhs 1) }

funccall:
| ID LPAREN RPAREN { Call($1, []) }
| ID LPAREN explist RPAREN { Call($1, $3) }
