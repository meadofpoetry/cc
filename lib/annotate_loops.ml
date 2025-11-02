open Parsetree

let annotate expr =
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
