%token <Ast.type_> TYPE
%token <Ast.lit> LIT
%token <string> IDENT

%token LET

%token PLUS
%token TIMES
%token GT
%token LT
%token EQ

%token LEFT_PAREN
%token RIGHT_PAREN
%token COLON
%token EQUALS

%token EOF

%left GT LT EQ
%left PLUS
%left TIMES

%start main
%type <unit Ast.stmt list> main

%%

main:
  | nonempty_list(stmt) EOF { $1 }

stmt:
  LET lvalue list(param) EQUALS expr {
    let id = $2 in
    let e = match $3 with
      | [] -> $5
      | xs -> List.fold_right
        (fun p acc -> Ast.Func((), p, acc))
        xs
        $5
    in
    Ast.Let (id, e)
  }

lvalue:
  | IDENT { Ast.LvalueUntyped ((), $1) }
  | IDENT COLON TYPE { Ast.LvalueTyped ((), $1, $3) }

param:
  | IDENT { Ast.LvalueUntyped ((), $1) }
  | LEFT_PAREN IDENT COLON TYPE RIGHT_PAREN { Ast.LvalueTyped ((), $2, $4) }

expr:
  | expr_app { $1 }
  | expr PLUS expr { Ast.Binop((), Ast.Add, $1, $3) }
  | expr TIMES expr { Ast.Binop((), Ast.Mul, $1, $3) }
  | expr GT expr { Ast.Binop((), Ast.Gt, $1, $3) }
  | expr LT expr { Ast.Binop((), Ast.Lt, $1, $3) }
  | expr EQ expr { Ast.Binop((), Ast.Eq, $1, $3) }

expr_app:
  | atomic_expr { $1 }
  | expr_app simple_expr { Ast.App((), $1, $2) }

atomic_expr:
  | simple_expr { $1 }

simple_expr:
  | LIT { Ast.Lit((), $1) }
  | IDENT { Ast.Rvalue(Ast.RvalueIdent((), $1)) }
  | LEFT_PAREN expr RIGHT_PAREN { $2 }