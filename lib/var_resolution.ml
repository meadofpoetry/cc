open Parsetree

exception Redeclaration : Id.t -> exn

exception Undeclared : Id.t -> exn

exception Invalid_lvalue : Parsetree.expr -> exn

module Env = struct
  module M = Map.Make(String)

  type 'a t = private 'a M.t list

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

let rec resolve env (PProgram f) =
  PProgram (resolve_fun_def env f)

and resolve_fun_def env (PFunction { name; body }) =
  PFunction { name; body = resolve_block env body }

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
  | PD var_decl  ->
     let var_decl', env' = resolve_var_decl env var_decl in
     PD var_decl', env'
  | PS statement ->
     PS (resolve_statement env statement), env

and resolve_var_decl env (PVar_decl (name, init)) =
  match Env.get_from_current name env with
  | Some _ ->
     raise (Redeclaration name)
  | None ->
     let unique_name, env' = Env.unique_alias name env in
     let init' = Option.map (resolve_expr env') init in
     PVar_decl (unique_name, init'), env'

and resolve_statement env = function
  | PReturn expr -> PReturn (resolve_expr env expr)
  | PIf { cond; _then; _else } ->
     PIf { cond = resolve_expr env cond
         ; _then = resolve_statement env _then
         ; _else = Option.map (resolve_statement env) _else
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
