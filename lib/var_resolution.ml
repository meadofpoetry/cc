open Parsetree

module Env = Map.Make(String)

exception Redeclaration : Id.t -> exn

exception Undeclared : Id.t -> exn

exception Invalid_lvalue : Parsetree.expr -> exn

let rec resolve env (PProgram f) =
  PProgram (resolve_fun_def env f)

and resolve_fun_def env (PFunction { name; body }) =
  let rec loop env acc = function
    | [] -> List.rev acc
    | item::rest ->
       let env', item' = resolve_block_item env item in
       loop env' (item'::acc) rest
  in
  PFunction { name; body = loop env [] body }

and resolve_block_item env = function
  | PD var_decl  ->
     let env', var_decl' = resolve_var_decl env var_decl in
     env', PD var_decl'
  | PS statement ->
     env, PS (resolve_statement env statement)

and resolve_var_decl env (PVar_decl (name, init)) =
  match Env.find_opt name env with
  | Some _ ->
     raise (Redeclaration name)
  | None ->
     let unique_name = Id.var name in
     let env' = Env.add name unique_name env in
     let init' = Option.map (resolve_expr env') init in
     env', PVar_decl (unique_name, init')

and resolve_statement env = function
  | PReturn expr -> PReturn (resolve_expr env expr)
  | PIf { cond; _then; _else } ->
     PIf { cond = resolve_expr env cond
         ; _then = resolve_statement env _then
         ; _else = Option.map (resolve_statement env) _else
       }
  | PExpr expr -> PExpr (resolve_expr env expr)
  | PNull -> PNull

and resolve_expr env = function
  | PVar name ->
     begin match Env.find_opt name env with
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
