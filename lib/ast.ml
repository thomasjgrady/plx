type 'a t = 'a stmt list
[@@deriving show]

and 'a stmt =
  | StmtExpr of 'a * 'a expr
  | StmtAssign of 'a * 'a lvalue * 'a expr
  | StmtAbstract of 'a * 'a lvalue list * 'a expr
[@@deriving show]

and 'a expr =
  | ExprLit of 'a * lit
  | ExprRvalue of 'a * rvalue
  | ExprParen of 'a * 'a expr
  | ExprTuple of 'a * 'a expr list
  | ExprStruct of 'a * (string * 'a expr) list
  | ExprEnum of 'a * (string * 'a expr) list
  | ExprApply of 'a * rvalue * 'a expr list
[@@deriving show]

and lit =
  | LitUnit
  | LitInt of int
  | LitBool of bool
  | LitString of string
  | LitType of type_
[@@deriving show]

and type_ =
  | TypeUnit
  | TypeInt
  | TypeBool
  | TypeString
  | TypeType (* todo: universes *)
[@@deriving show]

and 'a lvalue =
  | LvalueTyped of 'a * string * 'a expr
  | LvalueUntyped of 'a * string
[@@deriving show]

and rvalue =
  | RvalueIdent of string
  | RvalueProperty of string * string list
[@@deriving show]