open Parsetree

let rec tacky : Parsetree.program -> Tacky.program =
  fun (PProgram f) -> Tacky.Program (tacky_fun_decl f)

and tacky_fun_decl (PFunction { name; body }) =
  let instr = tacky_fun_body body in
  Tacky.Function { name; body = instr }

and tacky_fun_body body =
  let instr = Dynarray.create () in

  let rec statement = function
    | PReturn e ->
       let res = expr e in
       let i = Tacky.Return res in
       Dynarray.add_last instr i

  and expr = function
    | PConst i ->
       Tacky.Const i
    | PUn_op (op, inner) ->
       let src = expr inner in
       let dst = Tacky.Var (Id.temp ()) in
       let i = Tacky.Unary { op = (tacky_unop op); src = src; dst = dst } in
       Dynarray.add_last instr i;
       dst
  in
  
  ignore @@ statement body;
  Dynarray.to_list instr
  
and tacky_unop = function
  | PBit_neg -> Tacky.Complement
  | PNeg -> Tacky.Negate

let rec compile : Tacky.program -> X86.program =
  fun (Tacky.Program f) -> X86.Program (comp_fun_decl f)

and comp_fun_decl (Tacky.Function { name; body }) =
  let instr = List.concat_map comp_instr body in
  let alloc, instr' = replace_pseudo instr in
  let instr'' = fix_mov instr' in
  let instr''' = insert_prolog_epilog alloc instr'' in
  X86.Function { name; instr = instr''' }

and comp_instr = function
  | Tacky.Return v ->
     [ X86.Mov { src = val_to_reg v; dst = X86.Reg X86.RAX }
     ; X86.Ret
     ]
  | Tacky.Unary { op = Tacky.Complement; src; dst } ->
     [ X86.Mov { src = val_to_reg src; dst = val_to_reg dst }
     ; X86.Not (val_to_reg dst)
     ]
  | Tacky.Unary { op = Tacky.Negate; src; dst } ->
     [ X86.Mov { src = val_to_reg src; dst = val_to_reg dst }
     ; X86.Neg (val_to_reg dst)
     ]

and val_to_reg = function
  | Tacky.Const i -> X86.Imm i
  | Tacky.Var id -> X86.Pseudo id

and replace_pseudo : X86.instr list -> int * X86.instr list = fun instr ->
  let arg = ref 0 in
  let mapping = Hashtbl.create 10 in
  let replace = function
    | X86.Pseudo id ->
       begin match Hashtbl.find_opt mapping id with
       | Some off -> X86.Stack off
       | None ->
          incr arg;
          Hashtbl.add mapping id !arg;
          X86.Stack !arg
       end
    | other -> other
  in
  let fix_instr = function
    | X86.Mov { src; dst } -> X86.Mov { src = replace src; dst = replace dst }
    | X86.Sub { value; dst } -> X86.Sub { value = replace value; dst = replace dst }
    | X86.Neg op -> X86.Neg (replace op)
    | X86.Not op -> X86.Not (replace op)
    | X86.Pop op -> X86.Pop (replace op)
    | X86.Push op -> X86.Push (replace op)
    | other -> other
  in
  let instr' = List.map fix_instr instr in
  !arg, instr'

and fix_mov instr =
  let apply = function
    | X86.Mov { src = X86.Stack s; dst = X86.Stack d } ->
       [ X86.Mov { src = X86.Stack s; dst = X86.Reg X86.R10 }
       ; X86.Mov { src = X86.Reg X86.R10; dst = X86.Stack d }
       ]
    | other -> [ other ]
  in
  List.concat_map apply instr

and insert_prolog_epilog alloc instr =
  let rec replace_ret = function
    | [] -> []
    | X86.Ret :: rest ->
       X86.epilog @ X86.Ret :: replace_ret rest
    | op :: rest ->
       op :: replace_ret rest
  in
  X86.prolog alloc @ replace_ret instr
