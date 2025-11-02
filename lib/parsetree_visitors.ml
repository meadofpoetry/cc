open Parsetree

class virtual ['self] map =
object (self : 'self)
  method visit_loop_id _env loop_id =
    loop_id
               
  method visit_program env fun_decls =
    List.map (self#visit_fun_decl env) fun_decls

  method visit_decl env = function
    | PFun_decl fdecl -> PFun_decl (self#visit_fun_decl env fdecl)
    | PVar_decl vdecl -> PVar_decl (self#visit_var_decl env vdecl)

  method visit_fun_decl env { name; args; body } =
    { name; args; body = Option.map (self#visit_block env) body }

  method visit_var_decl env (name, expr) =
    (name, Option.map (self#visit_expr env) expr)
  
  method visit_block env item_list =
    List.map (self#visit_block_item env) item_list

  method visit_block_item env = function
    | PS s -> self#visit_PS env s
    | PD d -> self#visit_PD env d

  method visit_PS env statement =
    PS (self#visit_statement env statement)

  method visit_PD env decl =
    PD (self#visit_decl env decl)

  method visit_statement env = function
    | PReturn expr -> self#visit_PReturn env expr
    | PIf { cond; _then; _else } -> self#visit_PIf env cond _then _else
    | PExpr e -> self#visit_PExpr env e
    | PCompound b -> self#visit_PCompound env b
    | PBreak l -> self#visit_PBreak env l
    | PContinue l -> self#visit_PContinue env l
    | PWhile { cond; body; loop_id } -> self#visit_PWhile env cond body loop_id
    | PDoWhile { body; cond; loop_id } -> self#visit_PDoWhile env body cond loop_id
    | PFor { init; cond; post; body; loop_id } -> self#visit_PFor env init cond post body loop_id
    | PNull -> self#visit_PNull env

  method visit_PReturn env e =
    PReturn (self#visit_expr env e)

  method visit_PIf env cond _then _else =
    PIf { cond = self#visit_expr env cond
        ; _then = self#visit_statement env _then
        ; _else = Option.map (self#visit_statement env) _else
      }

  method visit_PExpr env expr =
    PExpr (self#visit_expr env expr)

  method visit_PCompound env block =
    PCompound (self#visit_block env block)

  method visit_PBreak env loop_id =
    PBreak (self#visit_loop_id env loop_id)

  method visit_PContinue env loop_id =
    PContinue (self#visit_loop_id env loop_id)

  method visit_PWhile env cond body loop_id =
    PWhile { cond = self#visit_expr env cond
           ; body = self#visit_statement env body
           ; loop_id = self#visit_loop_id env loop_id
      }

  method visit_PDoWhile env body cond loop_id =
    PDoWhile { body = self#visit_statement env body
             ; cond = self#visit_expr env cond
             ; loop_id = self#visit_loop_id env loop_id
      }

  method visit_PFor env init cond post body loop_id =
    PFor { init = Option.map (self#visit_for_init env) init
         ; cond = Option.map (self#visit_expr env) cond
         ; post = Option.map (self#visit_expr env) post
         ; body = self#visit_statement env body
         ; loop_id = self#visit_loop_id env loop_id
      }

  method visit_PNull _env =
    PNull

  method visit_for_init env = function
    | PInitDecl var_decl -> self#visit_PInitDecl env var_decl
    | PInitExpr expr -> self#visit_PInitExpr env expr

  method visit_PInitDecl env decl =
    PInitDecl (self#visit_var_decl env decl)

  method visit_PInitExpr env e =
    PInitExpr (self#visit_expr env e)
  
  method visit_expr env = function
    | PConst i -> self#visit_PConst env i
    | PVar id -> self#visit_PVar env id
    | PAssign (e1, e2) -> self#visit_PAssign env e1 e2
    | PFun_call (name, args) -> self#visit_PFun_call env name args
    | PUn_op (op, expr) -> self#visit_PUn_op env op expr
    | PBin_op (bin_op, e1, e2) -> self#visit_PBin_op env bin_op e1 e2
    | PTernary { cond; _then; _else } -> self#visit_PTernary env cond _then _else

  method visit_PConst _env i =
    PConst i

  method visit_PVar _env id =
    PVar id

  method visit_PAssign env e1 e2 =
    PAssign (self#visit_expr env e1, self#visit_expr env e2)

  method visit_PFun_call env name args =
    PFun_call (name, List.map (self#visit_expr env) args)

  method visit_PUn_op env op expr =
    PUn_op (op, self#visit_expr env expr)

  method visit_PBin_op env op e1 e2 =
    PBin_op (op, self#visit_expr env e1, self#visit_expr env e2)

  method visit_PTernary env cond _then _else =
    PTernary { cond = self#visit_expr env cond
             ; _then = self#visit_expr env _then
             ; _else = self#visit_expr env _else
      }
end
