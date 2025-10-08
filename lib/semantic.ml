open Parsetree

let fix_missing_return parsetree =
  let rec apply (PProgram f) =
    PProgram (apply_fun_def f)
  and apply_fun_def (PFunction { name; body }) =
    PFunction { name; body = apply_body body }
  and apply_body = function
    | [] -> [PS (PReturn (PConst 0))]
    | [PS (PReturn _)] as body -> body
    | h::tl -> h :: apply_body tl
  in
  apply parsetree

let validate parsetree =
  let parsetree' = fix_missing_return parsetree in
  Var_resolution.resolve Var_resolution.Env.empty parsetree'
