open Printf
open Ast

type value =
  | Lit of lit
  | Tuple of value list
  | Struct of (string * value) list
  | Enum of (string * value) list
  | Abstract of (env -> value list -> value)

and env = (string * value) list

let lookup env id = match List.assoc_opt id env with
  | None -> failwith (sprintf "Unknown identifier: %s" id)
  | Some x -> x

let rec eval_expr (ev : env) (e : 'a expr) = match e with
  | ExprLit (_, x) -> Lit x
  | ExprRvalue (_, rv) -> eval_rvalue ev rv
  | ExprParen (_, e') -> eval_expr ev e'
  | ExprTuple (_, es) -> Tuple (List.map (eval_expr ev) es)
  | ExprStruct (_, items) -> Struct (List.map (fun (k, v) -> (k, eval_expr ev v)) items)
  | ExprEnum (_, items) -> Enum (List.map (fun (k, v) -> (k, eval_expr ev v)) items)
  | ExprApply (_, rv, args) -> match (eval_rvalue ev rv) with
    | Abstract f ->
      let args' = List.map (eval_expr ev) args in
      f ev args'
    | _ -> failwith (sprintf "%s is not a function" (show_rvalue rv))

and eval_rvalue ev rv = match rv with
  | RvalueIdent id -> lookup ev id
  | RvalueProperty (id, keys) ->
    match lookup ev id with
      | Struct props -> eval_rvalue props (RvalueProperty (List.hd keys, List.tl keys))
      | _ -> failwith (sprintf "%s is not a struct" id)