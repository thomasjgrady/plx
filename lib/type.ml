open Printf

type t =
  | Type of Ast.type_
  | Func of t * t
  | Var of int

and context = (string * t) list
[@@deriving show, eq]

let var = ref 0

let new_type_var () =
  let v = !var in
  incr var ;
  Var v

let remove_shadowed ctx ids =
  List.filter (fun (name, _) -> not (List.exists (fun id -> id = name) ids)) ctx

let t_of_lit = function
  | Ast.Unit -> Type Ast.TypeUnit
  | Ast.Nat _ -> Type Ast.TypeNat
  | Ast.Bool _ -> Type Ast.TypeBool

let rec annotate_expr (ctx : context) (e : 'a Ast.expr) : t Ast.expr = match e with
  | Ast.Lit (_, x) -> Ast.Lit (t_of_lit x, x)
  | Ast.Rvalue rv -> Ast.Rvalue (annotate_rvalue ctx rv)
  | Ast.Binop (_, op, lhs, rhs) ->
    let lhs' = annotate_expr ctx lhs in
    let rhs' = annotate_expr ctx rhs in
    Ast.Binop(new_type_var (), op, lhs', rhs')
  | Ast.Func (_, lv, e') ->
    let lv' = annotate_lvalue lv in
    let lv_id = Ast.lvalue_id lv' in
    let lv_ty = Ast.lvalue_annotation lv' in
    let ctx' =  (lv_id, lv_ty) :: (remove_shadowed ctx [lv_id]) in
    let e'' = annotate_expr ctx' e' in
    Ast.Func (Func (lv_ty, Ast.expr_annotation e''), lv', e'')
  | Ast.App (_, e1, e2) ->
    let e1' = annotate_expr ctx e1 in
    let e2' = annotate_expr ctx e2 in
    Ast.App (new_type_var (), e1', e2')

and annotate_lvalue = function
  | Ast.LvalueTyped (_, id, ty) -> Ast.LvalueTyped (Type ty, id, ty)
  | Ast.LvalueUntyped (_, id) -> Ast.LvalueUntyped (new_type_var (), id)

and annotate_rvalue ctx rv = match rv with
  | Ast.RvalueIdent (_, id) ->
    let ty = match List.assoc_opt id ctx with
      | Some ty -> ty
      | None -> failwith (sprintf "Unknown identifier: %s" id)
    in
    Ast.RvalueIdent (ty, id)

let rec constraints (x : t Ast.expr) : (t * t) list = match x with
  | Ast.Lit _ -> []
  | Ast.Rvalue _ -> []
  | Ast.Binop (ty, op, lhs, rhs) ->
    let t_lhs = Ast.expr_annotation lhs in
    let t_rhs = Ast.expr_annotation rhs in
    let cons = match op with
      | Ast.Add | Ast.Mul -> [
        (ty, Type Ast.TypeNat);
        (t_lhs, Type Ast.TypeNat);
        (t_rhs, Type Ast.TypeNat)
      ]
      | Ast.Eq | Ast.Gt | Ast.Lt -> [
        (ty, Type Ast.TypeBool);
        (t_lhs, t_rhs)
      ]
    in
    (constraints lhs) @ (constraints rhs) @ cons
  | Ast.Func (ty, lv, e) ->
    let t_lv = Ast.lvalue_annotation lv in
    let t_e = Ast.expr_annotation e in
    let cons_body = constraints e in
    (match ty with
      | Func (t_arg, t_ret) -> [
          (t_arg, t_lv) ;
          (t_ret, t_e) ;
        ] @ cons_body
      | _ -> failwith (sprintf "Type error: not a function"))
  | Ast.App (ty, e1, e2) ->
    let cons = (constraints e1) @ (constraints e2) in
    (match ty with
    | Func (t_arg, t_ret) -> cons @ [(ty, t_ret); (Ast.expr_annotation e1, t_arg)]
    | Var _ -> cons @ [(Ast.expr_annotation e1, Func (Ast.expr_annotation e2, ty))]
    | _ -> failwith (sprintf "Invalid application: %s" (show ty)))

let rec substitute subs x =
  List.fold_right (fun (a, b) x' -> substitute_one a b x') subs x

(* replaces a with b in type ty *)
and substitute_one a b ty = match ty with
  (* primitive types have no replacement *)
  | Type _ -> ty
  (* if the given type equals the input replacement type,
  then replace *)
  | Var _ -> if a = ty then b else ty
  | Func (t_arg, t_ret) -> Func(substitute_one a b t_arg, substitute_one a b t_ret)

let rec unify (l : (t * t) list) = match l with
  | [] -> []
  | (x, y) :: xs ->
    let s1 = unify xs in
    let s2 = unify_one (substitute s1 x) (substitute s1 y)
    in
    s1 @ s2

and unify_one (x : t) (y : t) = match (x, y) with
  | (Type x, Type y) ->
    if x = y then [] else failwith (sprintf "Type error: mistmatched types. Got %s and %s" (Ast.show_type_ x) (Ast.show_type_ y))
  | (Var _, _) -> [(x, y)]
  | (_, Var _) -> [(y, x)]
  | (Func(a, b), Func(c, d)) -> unify [(a, c); (b, d)]
  | _ -> failwith (sprintf "Type error: mistmatched types %s and %s" (show x) (show y))

let rec apply_subs subs x =
  let s = substitute subs in
  match x with
    | Ast.Lit (ty, x) -> Ast.Lit (s ty, x)
    | Ast.Rvalue rv -> Ast.Rvalue (match rv with
      | Ast.RvalueIdent (ty, id) -> Ast.RvalueIdent (s ty, id))
    | Ast.Binop (ty, op, lhs, rhs) -> Ast.Binop (
      s ty ,
      op ,
      apply_subs subs lhs ,
      apply_subs subs rhs
    )
    | Ast.Func (ty, lv, e) ->
      let lv' = apply_lvalue_subs subs lv in
      Ast.Func (s ty, lv', apply_subs subs e)
    | Ast.App (ty, e1, e2) -> Ast.App (
      s ty ,
      apply_subs subs e1 ,
      apply_subs subs e2
    )

and apply_lvalue_subs subs lv = match lv with
  | Ast.LvalueTyped (ty', id, ty'') -> Ast.LvalueTyped (substitute subs ty', id, ty'')
  | Ast.LvalueUntyped (ty', id) -> Ast.LvalueUntyped (substitute subs ty', id)

let infer_expr ctx x =
  let a = annotate_expr ctx x in
  let cons = constraints a in
  let subs = unify cons in
  apply_subs subs a

let infer_stmt ctx s = match s with
  | Ast.Let (lv, e) ->
    let e' = infer_expr ctx e in
    let t_e = Ast.expr_annotation e' in
    let lv' = match lv with
      | Ast.LvalueTyped (_, id, ty) ->
        let ty' = Type ty in
        let subs = unify [(t_e, ty')] in
        Ast.LvalueTyped (substitute subs ty', id, ty)
      | Ast.LvalueUntyped (_, id) -> Ast.LvalueUntyped (t_e, id)
    in
    let lv_id = Ast.lvalue_id lv' in
    let ctx' = match List.assoc_opt lv_id ctx with
      | Some _ -> failwith (sprintf "Duplicate identifier: %s" lv_id)
      | None -> (lv_id, Ast.lvalue_annotation lv') :: ctx
    in
    (ctx', Ast.Let (lv', e'))

let infer ctx x =
  List.fold_left_map
  (fun ctx' s -> infer_stmt ctx' s)
  ctx
  x