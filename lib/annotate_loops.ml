open Parsetree

let annotate_visitor expr =
  let v = object (self)
    inherit [_] Parsetree_visitors.map

    method! visit_PBreak current_label _ =
      if Option.is_none current_label
      then failwith "break outside of the loop";
      PBreak current_label

    method! visit_PContinue current_label _ =
      if Option.is_none current_label
      then failwith "continue outside of the loop";
      PContinue current_label
    
    method! visit_PWhile _ cond body _ =
      let label = Id.label "while" in
      PWhile { cond = cond
             ; body = self#visit_statement (Some label) body
             ; loop_id = Some label
        }

    method! visit_PDoWhile _ body cond _ =
      let label = Id.label "do.while" in
      PDoWhile { body = self#visit_statement (Some label) body
               ; cond = cond
               ; loop_id = Some label
        }

    method! visit_PFor _ init cond post body _ =
      let label = Id.label "for" in
      PFor { init = init
           ; cond = cond
           ; post = post
           ; body = self#visit_statement (Some label) body
           ; loop_id = Some label
        }
    
    end
  in
  v#visit_program None expr

let rec annotate (PProgram fs) =
  PProgram (List.map annotate_fun_def fs)

and annotate_fun_def { name; args; body } =
  { name; args; body = Option.map (annotate_block None) body }

and annotate_block current_label (PBlock items) =
  PBlock (List.map (annotate_block_item current_label) items)

and annotate_block_item current_label = function
  | PD var_decl  ->
     PD var_decl
  | PS statement ->
     PS (annotate_statement current_label statement)

and annotate_statement current_label = function
  | PBreak _ ->
     if Option.is_none current_label
     then failwith "break outside of the loop";
     PBreak current_label
  | PContinue _ ->
     if Option.is_none current_label
     then failwith "continue outside of the loop";
     PContinue current_label
  | PIf { cond; _then; _else } ->
     PIf { cond = cond
         ; _then = annotate_statement current_label _then
         ; _else = Option.map (annotate_statement current_label) _else
       }
  | PWhile { cond; body; loop_id = _ } ->
     let label = Id.label "while" in
     PWhile { cond = cond
            ; body = annotate_statement (Some label) body
            ; loop_id = Some label
       }
  | PDoWhile { body; cond; loop_id = _ } ->
     let label = Id.label "do.while" in
     PDoWhile { body = annotate_statement (Some label) body
              ; cond = cond
              ; loop_id = Some label
       }
  | PFor { init; cond; post; body; loop_id = _ } ->
     let label = Id.label "for" in
     PFor { init = init
          ; cond = cond
          ; post = post
          ; body = annotate_statement (Some label) body
          ; loop_id = Some label
       }
  | PCompound block -> PCompound (annotate_block current_label block)
  | PExpr _ as e -> e
  | PReturn _ as ret -> ret
  | PNull -> PNull

