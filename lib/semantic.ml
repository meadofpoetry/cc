open Parsetree

let fix_missing_return parsetree =
  let rec apply (PProgram fs) =
    PProgram (List.map apply_fun_def fs)
  and apply_fun_def = function
    | { name = "main"; args; body = Some (PBlock statements) } ->
       { name = "main"; args; body = Some (PBlock (apply_body statements)) }
    | f -> f
  and apply_body = function
    | [] -> [PS (PReturn (PConst 0))]
    | [PS (PReturn _)] as body -> body
    | h::tl -> h :: apply_body tl
  in
  apply parsetree

let validate parsetree =
  let parsetree' = fix_missing_return parsetree in
  Var_resolution.resolve Var_resolution.Env.empty parsetree'
  |> Annotate_loops.annotate
