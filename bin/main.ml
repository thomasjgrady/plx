open Plx.Ast
open Printf

let program = [
  Let (
    LvalueUntyped ((), "f") ,
    Func (
      (),
      LvalueUntyped ((), "x"),
      Binop (
        () ,
        Eq ,
        Binop (
          () ,
          Add ,
          Rvalue (RvalueIdent ((), "x")) ,
          Lit ((), Nat 1)
        ) ,
        Lit ((), Nat 10)
      )
    )
  ) ;
  Let (
    LvalueTyped ((), "y", TypeBool) ,
    App (
      () ,
      Rvalue (RvalueIdent ((), "f")) ,
      Lit ((), Nat 1)
    )
  )
]

let () =
  print_newline () ;
  print_endline (String.concat "\n" (List.map (show_stmt pp_empty) program)) ;
  let (ctx, p) = Plx.Type.infer [] program in
  print_endline (String.concat "\n" (List.map (show_stmt Plx.Type.pp) p)) ;
  print_endline (String.concat "\n" (List.map (fun (id, ty) -> (sprintf "%s : %s" id (Plx.Type.show ty))) ctx)) ;