let string_of_token = function
  | Parser.TYPE t -> Printf.sprintf "TYPE(%s)" (Ast.show_type_ t)
  | Parser.LIT l -> Printf.sprintf "LIT(%s)" (Ast.show_lit l)
  | Parser.IDENT s -> Printf.sprintf "IDENT(%s)" s
  | Parser.LET -> "LET"
  | Parser.PLUS -> "PLUS"
  | Parser.TIMES -> "TIMES"
  | Parser.GT -> "GT" 
  | Parser.LT -> "LT"
  | Parser.EQ -> "EQ"
  | Parser.LEFT_PAREN -> "LEFT_PAREN"
  | Parser.RIGHT_PAREN -> "RIGHT_PAREN"
  | Parser.COLON -> "COLON"
  | Parser.EQUALS -> "EQUALS"
  | Parser.EOF -> "EOF"

let compile source =
  let lexbuf = Lexing.from_string source in
  
  (* Debug function to print all tokens *)
  let rec print_all_tokens () =
    try
      let token = Lexer.token lexbuf in
      Printf.printf "Token: %s\n" (string_of_token token);
      if token <> Parser.EOF then print_all_tokens ()
    with e -> 
      Printf.printf "Lexing error: %s\n" (Printexc.to_string e)
  in

  (* Print tokens first *)
  let _debug_lexbuf = Lexing.from_string source in
  print_all_tokens ();
  
  (* Reset lexbuf position for actual parsing *)
  let lexbuf = Lexing.from_string source in
  let p = Parser.main Lexer.token lexbuf in
  let (_, p') = Type.infer [] p in
  p'