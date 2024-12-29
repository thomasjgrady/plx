open Plx

let program : unit Ast.t = [
  Ast.Assign (
    () ,
    "x" ,
    Ast.Lit ((), Ast.Nat 10)
  ) ;
  Ast.Abs (
    () ,
    "f" ,
    ["y"] ,
    Ast.Binop (
      () ,
      Ast.Add ,
      Ast.Ident ((), "y") ,
      Ast.Lit ((), Ast.Nat 1)
    )
  ) ;
  Ast.Assign (
    () ,
    "y" ,
    Ast.App ((), "f", [Ast.Lit ((), Ast.Nat 1)])
  )
]

let () =
  let (env, _res) = Eval.eval [] program in
  print_newline () ;
  print_endline (Ast.pp program) ;
  print_endline (Eval.show_env env) ;
  