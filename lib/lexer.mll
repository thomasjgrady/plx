{
  open Parser
}

let whitespace = [' ' '\n' '\t' '\r']
let digit = ['0'-'9']
let nat = digit+

let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit | '\'' | '_')*

rule token = parse
  | whitespace+ { token lexbuf }
  | nat as n { LIT (Ast.Nat (int_of_string n)) }
  | "let" { LET }
  | "true" { LIT (Ast.Bool true) }
  | "false" { LIT (Ast.Bool false) }
  | "nat" { TYPE Ast.TypeNat }
  | "bool" { TYPE Ast.TypeBool }
  | "unit" { TYPE Ast.TypeUnit }
  | "()" { LIT (Ast.Unit) }
  | "+" { PLUS }
  | "*" { TIMES }
  | ">" { GT }
  | "<" { LT }
  | "==" { EQ }
  | "=" { EQUALS }
  | "(" { LEFT_PAREN }
  | ")" { RIGHT_PAREN }
  | ":" { COLON }
  | ident as id { IDENT id }
  | eof { EOF }