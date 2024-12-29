type value =
  | Lit of Ast.lit
  | Abs of (env -> value list -> value)
[@@deriving show]

and env = (string * value) list
[@@deriving show]

let lookup env id = match List.assoc_opt id env with
  | None -> failwith (Printf.sprintf "Unknown identifier: %s" id)
  | Some x -> x

let algebra : (env, value) Ast.algebra = {
  expr = {
    lit = (fun env x -> (env, Lit x)) ;
    ident = (fun env id -> (env, lookup env id)) ;
    app = (fun env id args -> (match lookup env id with
      | Abs f -> (env, f env args)
      | _ -> failwith (Printf.sprintf "%s is not a function" id))) ;
    binop = (fun env op x y ->
      let (x', y') = match (x, y) with
        | (Lit Ast.Nat x', Lit Ast.Nat y') -> (x', y')
        | _ -> failwith (Printf.sprintf "type error")
      in
      let z = match op with
        | Add -> x' + y'
        | Mul -> x' * y'
      in
      (env, Lit (Ast.Nat z))) ;
  } ;
  stmt = {
    assign = (fun env id x -> (match List.assoc_opt id env with
      | None -> ((id, x) :: env, Lit Ast.Unit)
      | Some _ -> failwith (Printf.sprintf "use of duplicate identifier: %s" id))) ;
    abs = (fun env id args thunk -> (match List.assoc_opt id env with
      | None ->
        let f (env' : env) args' =
          if List.length args <> List.length args'
          then failwith "invalid number of arguments"
          (* todo: recursion *)
          else
            let captured = List.filter
              (fun (id', _) -> match List.find_opt (fun x -> x = id') args with
                | None -> true
                | _ -> false)
              env'
            in
            thunk (List.concat [List.combine args args'; captured])
        in
        ((id, Abs f) :: env, Lit Ast.Unit)
      | Some _ -> failwith (Printf.sprintf "use of duplicate identifier: %s" id))) ;
  } ;
  prog = (fun env _rs -> (env, Lit Ast.Unit)) ;
}

let eval env program = Ast.fold algebra env program