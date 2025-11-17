
type ty = TyInt
        | TyFun of ty list


module Symbols : sig

  type attr = AttrFun of { defined : bool; global : bool }
            | AttrStatic of { init : init_value; global : bool }
            | AttrLocal
  and init_value = Tentative | Init of int | NoInit

  val clear : unit -> unit
  
  val add : name:string -> attr:attr -> ty -> unit

  val get : name:string -> (ty * attr) option

  val get_exn : name:string -> ty * attr
  
  val get_type : name:string -> ty option

  val get_type_exn : name:string -> ty

  val to_seq : unit -> (string * (ty * attr)) Seq.t
  
  val is_defined : string -> bool

  val is_global : string -> bool
  
end = struct

  type attr = AttrFun of { defined : bool; global : bool }
            | AttrStatic of { init : init_value; global : bool }
            | AttrLocal
  and init_value = Tentative | Init of int | NoInit

  let symbols : (string, (ty * attr)) Hashtbl.t = Hashtbl.create 16

  let clear () =
    Hashtbl.clear symbols
  
  let add ~name ~attr ty =
    Hashtbl.replace symbols name (ty, attr)

  let get ~name =
    Hashtbl.find_opt symbols name

  let get_exn ~name =
    match get ~name with
    | Some v -> v
    | None -> failwith (Printf.sprintf "Symbol not found: %s" name)
  
  let get_type ~name =
    Hashtbl.find_opt symbols name
    |> Option.map fst

  let get_type_exn ~name =
    get_exn ~name |> fst

  let to_seq () =
    Hashtbl.to_seq symbols
  
  let is_defined name =
    match Hashtbl.find_opt symbols name with
    | Some (TyFun _, (AttrFun { defined; global = _ })) ->
       defined
    | _ ->
       false

  let is_global name =
    match Hashtbl.find_opt symbols name with
    | Some (_, (AttrFun { defined = _; global })) ->
       global
    | Some (_, (AttrStatic { init = _; global })) ->
       global
    | _ ->
       false
  
end

let is_static name =
  match Symbols.get ~name with
  | Some (_, Symbols.AttrStatic _) -> true
  | _ -> false
  

let var_init name =
  match Symbols.get ~name with
  | Some (_, (Symbols.AttrStatic { init; global = _ })) -> Some init
  | _ -> None

let run parsetree =
  let v =
    object (self)
      inherit [_] Parsetree_visitors.map as super

      (* Workaround to distinct bw toplevel and local var decls *)
      method! visit_program () decls =
        let open Parsetree in
        let process_decl = function
          | PFun_decl fdecl -> PFun_decl (self#visit_fun_decl () fdecl)
          | PVar_decl vdecl -> PVar_decl (self#toplevel_var_decl vdecl)
        in
        List.map process_decl decls
      
      method! visit_fun_decl () { name; args; body; storage_class } =
        let ty = TyFun (List.map (fun _ -> TyInt) args) in
        let global = ref (storage_class <> Some PStatic) in

        Symbols.get_type ~name
        |> Option.iter (fun prev_decl ->
               if prev_decl <> ty
               then failwith "Incompatible function declarations";

               if Symbols.is_defined name && Option.is_some body
               then failwith "Function redefinition";

               if Symbols.is_global name && storage_class = Some PStatic
               then failwith "Static function declaration follows non-static";
               
               global := Symbols.is_global name);

        let attr = Symbols.AttrFun { defined = Symbols.is_defined name || Option.is_some body
                                   ; global = !global
                                   }
        in
        Symbols.add ~name ~attr ty;
        body
        |> Option.iter (fun _ ->
               List.iter (fun param -> Symbols.add ~name:param ~attr:Symbols.AttrLocal TyInt) args);
        
        super#visit_fun_decl () { name; args; body; storage_class }

      method toplevel_var_decl Parsetree.{ name; init; storage_class } =
        let default_init = match init with
          | Some (PConst i) -> Symbols.Init i
          | None when storage_class = Some PExtern -> Symbols.NoInit
          | None -> Symbols.Tentative
          | Some _ -> failwith "Non-constant initializer"
        in
        let global = ref (storage_class <> Some PStatic) in

        Symbols.get_type ~name
        |> Option.iter (fun prev_ty ->
               if prev_ty <> TyInt
               then failwith "Function redeclared as var";

               if storage_class = Some PExtern
               then global := Symbols.is_global name
               else if Symbols.is_global name <> !global
               then failwith "Conflicting var linkage");
        
        let attr_init = match var_init name, default_init with
          | Some (Symbols.Init _), Symbols.Init _ ->
             failwith "Conflicting var decl"
          | Some (Symbols.Init i), _ ->
             Symbols.Init i
          | Some Symbols.Tentative, (Symbols.NoInit | Symbols.Tentative) ->
             Symbols.Tentative
          | _ ->
             default_init
        in
        let attr = Symbols.AttrStatic { init = attr_init; global = !global } in
        Symbols.add ~name ~attr TyInt;
        
        super#visit_var_decl () { name; init; storage_class }

      (* Local var decls *)
      method! visit_var_decl () { name; init; storage_class } =
        let open Parsetree in
        begin match storage_class with
        | Some PExtern ->
           if Option.is_some init
           then failwith "Initialized on local extern var";

           begin match Symbols.get_type ~name with
           | Some prev_ty ->
              if prev_ty <> TyInt
              then failwith "Redeclared local var type"
           | None ->
              let attr = Symbols.AttrStatic { init = Symbols.NoInit; global = true } in
              Symbols.add ~name ~attr TyInt
           end
        | Some PStatic ->
           let init = match init with
             | Some (PConst i) ->
                Symbols.Init i
             | None ->
                Symbols.Init 0
             | _ ->
                failwith "Non-constant init on local static var"
           in
           let attr = Symbols.AttrStatic { init; global = false } in
           Symbols.add ~name ~attr TyInt
        | None ->
           Symbols.add ~name ~attr:Symbols.AttrLocal TyInt
        end;
        super#visit_var_decl () { name; init; storage_class }

      method! visit_PInitDecl env decl =
        if Option.is_some decl.storage_class
        then failwith "For-loop var declaration can't contain storage class";
        PInitDecl (self#visit_var_decl env decl)
      
      method! visit_PVar () id =
        begin match Symbols.get_type_exn ~name:id with
        | TyInt -> ()
        | _ -> failwith "Function name used as var"
        end;
        super#visit_PVar () id
      
      method! visit_PFun_call () name args =
        begin match Symbols.get_type_exn ~name with
        | TyInt -> failwith "Variable called as function"
        | TyFun tys when List.length tys <> List.length args ->
           failwith "Wrong number of arguments"
        | TyFun _ -> ()
        end;
        super#visit_PFun_call () name args
      
    end
  in
  Symbols.clear ();
  v#visit_program () parsetree
