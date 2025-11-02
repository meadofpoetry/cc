open Parsetree

exception Redeclaration : Id.t -> exn

exception Undeclared : Id.t -> exn

exception Undeclared_fun : Id.t -> exn

exception Invalid_lvalue : Parsetree.expr -> exn

exception Inner_fun_definition : Id.t -> exn

module Scoped_env = struct

  type 'a t = (string, 'a) Hashtbl.t Stack.t

  let make = Stack.create

  let push_scope t =
    Stack.push (Hashtbl.create 16) t

  let pop_scope t =
    ignore @@ Stack.pop t
  
  let with_scope ?(env = make ()) f =
    push_scope env;
    let res = f env in
    pop_scope env;
    res

  let depth env =
    Stack.length env

  let get_from_current t key =
    Hashtbl.find_opt (Stack.top t) key

  let add t key value =
    Hashtbl.add (Stack.top t) key value

  let find t key =
    Stack.to_seq t
    |> Seq.find_map (fun tbl -> Hashtbl.find_opt tbl key)
  
end

type entry = { name : Id.t
             ; has_linkage : bool
             }

let make_entry ?(has_linkage = false) name =
  { name; has_linkage }

let unique_alias var_name = make_entry (Id.var var_name)

let resolve program =
  let v =
    object (self)
      inherit [_] Parsetree_visitors.map

      method resolve_param env name =
        match Scoped_env.get_from_current env name with
        | Some _ ->
           raise (Redeclaration name)
        | None ->
           let alias = unique_alias name in
           Scoped_env.add env name alias;
           Printf.printf "adding %s\n" name;
           alias.name

      (* special constructor since we create scope
       * within visit_fun_decl
       *)
      method resolve_block_no_new_scope env items =
        List.map (self#visit_block_item env) items
        
      method! visit_block env items =
        Scoped_env.with_scope ~env (fun inner_env ->
            self#resolve_block_no_new_scope inner_env items)

      method! visit_fun_decl env { name; args; body } =
        (* check if not multiple redeclaration of the fun with internal linkage *)
        Scoped_env.find env name
        |> Option.iter (fun prev_entry ->
               if Option.is_some (Scoped_env.get_from_current env name)
                  && (not prev_entry.has_linkage)
               then raise (Redeclaration name));

        (* check if it's inner function definition *)
        body
        |> Option.iter (fun _ ->
               if Scoped_env.depth env > 1
               then raise (Inner_fun_definition name));

        Scoped_env.add env name (make_entry ~has_linkage:true name);

        Scoped_env.with_scope ~env (fun inner_env ->
            let args' = List.map (self#resolve_param inner_env) args in
            let body' = Option.map (self#resolve_block_no_new_scope inner_env) body in
            { name; args = args'; body = body' })
      
      method! visit_var_decl env (name, init) =
        match Scoped_env.get_from_current env name with
        | Some _ ->
           raise (Redeclaration name)
        | None ->
           let alias = unique_alias name in
           Scoped_env.add env name alias;
           let init' = Option.map (self#visit_expr env) init in
           (alias.name, init')

      method! visit_PVar env name =
        match Scoped_env.find env name with
        | None -> raise (Undeclared name)
        | Some unique -> PVar unique.name

      method! visit_PAssign env v exp =
        match v with
        | PVar _ ->
           PAssign (self#visit_expr env v, self#visit_expr env exp)
        | lvalue ->
           raise (Invalid_lvalue lvalue)

      method! visit_PFor env init cond post body loop_id =
        let make_PFor env init =
          PFor { init = init
               ; cond = Option.map (self#visit_expr env) cond
               ; post = Option.map (self#visit_expr env) post
               ; body = self#visit_statement env body
               ; loop_id = self#visit_loop_id env loop_id
            }
        in
        match init with
          | Some PInitExpr e ->
             let init = PInitExpr (self#visit_expr env e) in
             make_PFor env (Some init)
          | Some PInitDecl d ->
             Scoped_env.with_scope ~env (fun inner_env ->
                 let init = PInitDecl (self#visit_var_decl inner_env d) in
                 make_PFor inner_env (Some init))
          | None ->
             make_PFor env None

      method! visit_PFun_call env name args =
        match Scoped_env.find env name with
        | None ->
           raise (Undeclared_fun name)
        | Some fun_alias ->
           let args' = List.map (self#visit_expr env) args in
           PFun_call (fun_alias.name, args')
      
    end
  in
  Scoped_env.with_scope (fun env ->
      v#visit_program env program)
