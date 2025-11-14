open Parsetree

let fix_missing_return parsetree =
  let v = object (self)
     inherit [_] Parsetree_visitors.map as super
     method! visit_fun_decl () = function
       | { name; args; body = Some statements; storage_class } ->
          { name
          ; args
          ; body = Some (self#append_return statements)
          ; storage_class
          }
       | decl -> super#visit_fun_decl () decl

     method append_return = function
       | [] -> [PS (PReturn (PConst 0))]
       | [PS (PReturn _)] as body -> body
       | h::tl -> h :: self#append_return tl
  end
  in
  v#visit_program () parsetree

let validate parsetree =
  let parsetree' = fix_missing_return parsetree in
  Var_resolution.resolve parsetree'
  |> Annotate_loops.annotate
  |> Typecheck.run
