type lit =
  | Nat of int
  | Bool of bool
  | Unit
[@@deriving show]

type binop =
  | Add
  | Mul

type 'a expr =
  | Lit of 'a * lit
  | Ident of 'a * string
  | App of 'a * string * 'a expr list
  | Binop of 'a * binop * 'a expr * 'a expr

type 'a stmt =
  | Assign of 'a * string * 'a expr
  | Abs of 'a * string * string list * 'a expr

type 'a t = 'a stmt list

type ('env, 'repr) expr_algebra = {
  lit : 'env -> lit -> 'env * 'repr;
  ident : 'env -> string -> 'env * 'repr;
  app : 'env -> string -> 'repr list -> 'env * 'repr;
  binop : 'env -> binop -> 'repr -> 'repr -> 'env * 'repr;
}

type ('env, 'repr) stmt_algebra = {
  assign : 'env -> string -> 'repr -> 'env * 'repr;
  abs : 'env -> string -> string list -> ('env -> 'repr) -> 'env * 'repr;
}

type ('env, 'repr) algebra = {
  expr : ('env, 'repr) expr_algebra ;
  stmt : ('env, 'repr) stmt_algebra ;
  prog : 'env -> 'repr list -> 'env * 'repr ;
}

let rec fold (alg : ('env, 'repr) algebra) (env : 'env) (x : 'a t) : 'env * 'repr =
  let (env', rs) = List.fold_left
    (fun (env', rs) s -> let (env'', r) = fold_stmt alg env' s in (env'', r :: rs))
    (env, [])
    x
  in
  alg.prog env' (List.rev rs)

and fold_stmt (alg : ('env, 'repr) algebra) (env : 'env) (x : 'a stmt) : 'env * 'repr = match x with
  | Assign (_, id, e) ->
    let (env', r) = fold_expr alg env e in
    alg.stmt.assign env' id r
  | Abs (_, id, args, body) ->
    let thunk env' = snd (fold_expr alg env' body) in
    alg.stmt.abs env id args thunk

and fold_expr (alg : ('env, 'repr) algebra) (env : 'env) (x : 'a expr) : 'env * 'repr = match x with
  | Lit (_, x) -> alg.expr.lit env x
  | Ident (_, id) -> alg.expr.ident env id
  | App (_, id, args) ->
    let (env', args') = List.fold_left
      (fun (env', args') e ->
        let (env'', arg) = fold_expr alg env' e in
        (env'', arg :: args')
      )
      (env, [])
      args
    in
    alg.expr.app env' id (List.rev args')
  | Binop (_, op, x, y) ->
    let (env', a) = fold_expr alg env x in
    let (env'', b) = fold_expr alg env' y in
    alg.expr.binop env'' op a b

let rec map (f_stmt : 'a stmt -> 'b) (f_expr : 'a expr -> 'b) (x : 'a t) : 'b t =
  List.map (map_stmt f_stmt f_expr) x

and map_stmt f_stmt f_expr s =
  let b = f_stmt s in
  let m = map_expr f_expr in
  match s with
    | Assign (_, id, x) -> Assign (b, id, m x)
    | Abs (_, id, args, x) -> Abs (b, id, args, m x)

and map_expr f e =
  let b = f e in
  let m = map_expr f in
  match e with
    | Lit (_, x) -> Lit (b, x)
    | Ident (_, id) -> Ident (b, id)
    | App (_, id, args) -> App (b, id, List.map m args)
    | Binop (_, op, x, y) -> Binop (b, op, m x, m y)

let pp_algebra : (unit, string) algebra = {
  expr = {
    lit = (fun env x -> (env, match x with
      | Nat n -> string_of_int n
      | Bool b -> string_of_bool b
      | Unit -> "()")) ;
    ident = (fun env id -> (env, id)) ;
    app = (fun env id args -> (env, id ^ " " ^ (String.concat " " args))) ;
    binop = (fun env op x y -> (
      env ,
      Printf.sprintf "(%s %s %s)"
        x
        (match op with
          | Add -> "+"
          | Mul -> "*")
        y
    )) ;
  } ;
  stmt = {
    assign = (fun env id x -> (env, Printf.sprintf "let %s = %s" id x)) ;
    abs = (fun env id args f -> (
      env ,
      Printf.sprintf "let %s %s = %s"
      id
      (String.concat " " args)
      (f ()) (* eager eval to print immediately *)
    )) ;
  } ;
  prog = (fun env xs -> (env, String.concat "\n" xs)) ;
}

let pp x = snd (fold pp_algebra () x)