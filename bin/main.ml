open Plx.Ast

let expr = Func (
  (),
  LvalueUntyped ((), "x"),
  Binop (
    () ,
    Eq ,
    Lit ((), Nat 10) ,
    Binop (
      (),
      Add,
      Rvalue (RvalueIdent ((), "x")),
      Lit ((), Nat 1)
    )
  )
  
)

let () =
  print_newline() ;
  print_endline (show_expr pp_empty expr) ;
  let x = Plx.Type.infer_expr [] expr in
  print_endline (show_expr Plx.Type.pp x)