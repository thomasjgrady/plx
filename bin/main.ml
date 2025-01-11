open Plx.Compiler

let source = {|
  let f x y = x * y + 1
|}

let () =
  let ast = compile source in
  print_endline (String.concat "\n" (List.map (Plx.Ast.show_stmt Plx.Type.pp) ast)) ;