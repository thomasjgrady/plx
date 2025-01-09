type type_ =
  | TypeUnit
  | TypeNat
  | TypeBool
[@@deriving show, eq]

and 'a stmt =
  | Let of 'a lvalue * 'a expr

and 'a lvalue =
  | LvalueTyped of 'a * string * type_
  | LvalueUntyped of 'a * string


and 'a expr =
  | Lit of 'a * lit
  | Rvalue of 'a rvalue
  | Binop of 'a * binop * 'a expr * 'a expr
  | Func of 'a * 'a lvalue * 'a expr
  | App of 'a * 'a expr * 'a expr

and lit =
  | Unit
  | Nat of int
  | Bool of bool

and 'a rvalue =
  | RvalueIdent of 'a * string

and binop =
  | Add
  | Mul
  | Eq
  | Gt
  | Lt
[@@deriving show]

let lvalue_id = function
  | LvalueTyped (_, id, _) -> id
  | LvalueUntyped (_, id) -> id

let rvalue_id = function
  | RvalueIdent (_, id) -> id

let lvalue_annotation = function
  | LvalueTyped (a, _, _) -> a
  | LvalueUntyped (a, _) -> a

let rvalue_annotation = function
  | RvalueIdent (a, _) -> a

let expr_annotation = function
  | Lit (a, _) -> a
  | Rvalue (RvalueIdent (a, _)) -> a
  | Binop (a, _, _, _) -> a
  | Func (a, _, _) -> a
  | App (a, _, _) -> a

type empty = unit
[@@deriving show]