open Parsetree

exception Redeclaration : Id.t -> exn

exception Undeclared : Id.t -> exn

exception Invalid_lvalue : Parsetree.expr -> exn

module Env = struct
  module M = Map.Make(String)

  type 'a t = 'a M.t list

  let empty = [M.empty]

  let child_block env = M.empty :: env

  let get k =
    List.find_map (M.find_opt k)

  let get_from_current k = function
    | [] -> failwith "unreachable"
    | env :: _ -> M.find_opt k env

  let unique_alias k = function
    | [] -> failwith "unreachable"
    | env :: rest ->
       let depth = List.length rest in
       let unique_name = Id.var (Printf.sprintf "%s_b%d_" k depth) in
       unique_name, (M.add k unique_name env)::rest
  
end

let rec resolve env (PProgram fs) =
  PProgram (List.map (resolve_fun_def env) fs)

and resolve_fun_def env { name; args; body } =
  { name; args; body = Option.map (resolve_block env) body }

and resolve_block env (PBlock items) =
  let env' = Env.child_block env in
  let rec loop env acc = function
    | [] -> List.rev acc
    | item::rest ->
       let item', env' = resolve_block_item env item in
       loop env' (item'::acc) rest
  in
  PBlock (loop env' [] items)

and resolve_block_item env = function
  | PD (PVar_decl var_decl) ->
     let var_decl', env' = resolve_var_decl env var_decl in
     PD (PVar_decl var_decl'), env'
  | PD (PFun_decl _) -> failwith "TODO"
  | PS statement ->
     PS (resolve_statement env statement), env

and resolve_var_decl env (name, init) : var_decl * string Env.t =
  match Env.get_from_current name env with
  | Some _ ->
     raise (Redeclaration name)
  | None ->
     let unique_name, env' = Env.unique_alias name env in
     let init' = Option.map (resolve_expr env') init in
     ((unique_name, init') : var_decl), env'

and resolve_statement env = function
  | PReturn expr -> PReturn (resolve_expr env expr)
  | PBreak _ as s -> s
  | PContinue _ as s -> s
  | PIf { cond; _then; _else } ->
     PIf { cond = resolve_expr env cond
         ; _then = resolve_statement env _then
         ; _else = Option.map (resolve_statement env) _else
       }
  | PWhile { cond; body; loop_id } ->
     PWhile { cond = resolve_expr env cond
            ; body = resolve_statement env body
            ; loop_id
       }
  | PDoWhile { body; cond; loop_id } ->
     PDoWhile { body = resolve_statement env body
              ; cond = resolve_expr env cond
              ; loop_id
       }
  | PFor { init; cond; post; body; loop_id } ->
     let init', env' = match init with
       | Some PInitExpr e ->
          Some (PInitExpr (resolve_expr env e)), env
       | Some PInitDecl d ->
          let inner_env = Env.child_block env in 
          let decl, env' = resolve_var_decl inner_env d in
          Some (PInitDecl decl), env'
       | None -> None, env
     in
     PFor { init = init'
          ; cond = Option.map (resolve_expr env') cond
          ; post = Option.map (resolve_expr env') post
          ; body = resolve_statement env' body
          ; loop_id
       }
  | PCompound block -> PCompound (resolve_block env block)
  | PExpr expr -> PExpr (resolve_expr env expr)
  | PNull -> PNull

and resolve_expr env = function
  | PVar name ->
     begin match Env.get name env with
     | None -> raise (Undeclared name)
     | Some unique -> PVar unique
     end
  | PAssign (PVar _ as v, expr) ->
     PAssign (resolve_expr env v, resolve_expr env expr)
  | PAssign (lvalue, _) ->
     raise (Invalid_lvalue lvalue)
  | PTernary { cond; _then; _else } ->
     PTernary { cond = resolve_expr env cond
              ; _then = resolve_expr env _then
              ; _else = resolve_expr env _else
       }
  | PConst _ as expr -> expr
  | PUn_op (op, expr) ->
     PUn_op (op, resolve_expr env expr)
  | PBin_op (op, left, right) ->
     PBin_op (op, resolve_expr env left, resolve_expr env right)
  | _ -> failwith "TODO"
