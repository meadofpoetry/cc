
type ty = TyInt
        | TyFun of ty list

let symbols : (string, ty) Hashtbl.t = Hashtbl.create 16

let fun_def : (string, bool) Hashtbl.t = Hashtbl.create 16

let is_defined name =
  Hashtbl.find_opt fun_def name
  |> Option.value ~default:false

let run parsetree =
  let v =
    object
      inherit [_] Parsetree_visitors.map as super

      method! visit_fun_decl () { name; args; body } =
        let ty = TyFun (List.map (fun _ -> TyInt) args) in

        Hashtbl.find_opt symbols name
        |> Option.iter (fun prev_decl ->
               if prev_decl <> ty
               then failwith "Incompatible function declarations";

               if is_defined name && Option.is_some body
               then failwith "Function redefinition");

        Hashtbl.add symbols name ty;
        body
        |> Option.iter (fun _ ->
               Hashtbl.add fun_def name true;
               List.iter (fun param -> Hashtbl.add symbols param TyInt) args);
        
        super#visit_fun_decl () { name; args; body }
      
      method! visit_var_decl () (name, expr) =
        Hashtbl.add symbols name TyInt;
        super#visit_var_decl () (name, expr)

      method! visit_PVar () id =
        begin match Hashtbl.find symbols id with
        | TyInt -> ()
        | _ -> failwith "Function name used as var"
        end;
        super#visit_PVar () id
      
      method! visit_PFun_call () name args =
        begin match Hashtbl.find symbols name with
        | TyInt -> failwith "Variable called as function"
        | TyFun tys when List.length tys <> List.length args ->
           failwith "Wrong number of arguments"
        | TyFun _ -> ()
        end;
        super#visit_PFun_call () name args
      
    end
  in
  Hashtbl.clear symbols;
  Hashtbl.clear fun_def;
  v#visit_program () parsetree
