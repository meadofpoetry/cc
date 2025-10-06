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
       let i = Tacky.Unary { op = tacky_unop op; src = src; dst = dst } in
       Dynarray.add_last instr i;
       dst
    | PBin_op (PAnd, left, right) ->
       let v1 = expr left in
       let false_label = Id.label "and_false" in
       Dynarray.add_last instr (Tacky.JumpIfZero{ cond = v1; target = false_label });
       let v2 = expr right in
       Dynarray.add_last instr (Tacky.JumpIfZero{ cond = v2; target = false_label });
       let result = Id.temp () in
       Dynarray.add_last instr (Tacky.Copy { src = Const 1; dst = Var result });
       let end_label = Id.label "and_end" in
       Dynarray.add_last instr (Tacky.Jump end_label);
       Dynarray.add_last instr (Tacky.Label false_label);
       Dynarray.add_last instr (Tacky.Copy { src = Const 0; dst = Var result });
       Dynarray.add_last instr (Tacky.Label end_label);
       Var result
    | PBin_op (POr, left, right) ->
       let v1 = expr left in
       let true_label = Id.label "or_true" in
       Dynarray.add_last instr (Tacky.JumpIfNotZero{ cond = v1; target = true_label });
       let v2 = expr right in
       Dynarray.add_last instr (Tacky.JumpIfNotZero{ cond = v2; target = true_label });
       let result = Id.temp () in
       Dynarray.add_last instr (Tacky.Copy { src = Const 0; dst = Var result });
       let end_label = Id.label "or_end" in
       Dynarray.add_last instr (Tacky.Jump end_label);
       Dynarray.add_last instr (Tacky.Label true_label);
       Dynarray.add_last instr (Tacky.Copy { src = Const 1; dst = Var result });
       Dynarray.add_last instr (Tacky.Label end_label);
       Var result
    | PBin_op (op, left, right) ->
       let src1 = expr left in
       let src2 = expr right in
       let dst  = Tacky.Var (Id.temp ()) in
       let i = Tacky.Binary { op = tacky_binop op; src1; src2; dst } in
       Dynarray.add_last instr i;
       dst
  in
  
  ignore @@ statement body;
  Dynarray.to_list instr

and tacky_unop = function
  | PBit_neg -> Tacky.Complement
  | PNeg -> Tacky.Negate
  | PNot -> Tacky.Not

and tacky_binop = function
  | PAdd -> Tacky.Add
  | PSub -> Tacky.Sub
  | PMult -> Tacky.Mult
  | PDiv -> Tacky.Div
  | PRem -> Tacky.Rem
  | PEqual -> Tacky.Equal
  | PNotEqual -> Tacky.NotEqual
  | PLessThan -> Tacky.LessThan
  | PLessEqual -> Tacky.LessEqual
  | PGreaterThan -> Tacky.GreaterThan
  | PGreaterEqual -> Tacky.GreaterEqual
  | PAnd | POr -> failwith "unreachable"

let rec compile : Tacky.program -> X86.program =
  fun (Tacky.Program f) -> X86.Program (comp_fun_decl f)

and comp_fun_decl (Tacky.Function { name; body }) =
  let instr = List.concat_map comp_instr body in
  let alloc, instr' = replace_pseudo instr in
  let instr'' = fix_instr instr' in
  let instr''' = insert_prolog_epilog alloc instr'' in
  X86.Function { name; instr = instr''' }

and comp_instr = function
  | Tacky.Return v ->
     [ X86.Mov { src = val_to_reg v; dst = X86.Reg X86.RAX }
     ; X86.Ret
     ]
  | Tacky.Unary { op; src; dst } ->
     comp_unary op src dst
  | Tacky.Binary { op; src1; src2; dst } ->
     comp_binary op src1 src2 dst
  | Tacky.Copy { src; dst } ->
     [ X86.Mov { src = val_to_reg src; dst = val_to_reg dst } ]
  | Tacky.Jump label ->
     [ X86.Jmp label ]
  | Tacky.JumpIfZero { cond; target } ->
     [ X86.Cmp (X86.Imm 0, val_to_reg cond)
     ; X86.Je target
     ]
  | Tacky.JumpIfNotZero { cond; target } ->
     [ X86.Cmp (X86.Imm 0, val_to_reg cond)
     ; X86.Jne target
     ]
  | Tacky.Label label ->
     [ X86.Label label ]

and comp_unary op src dst =
  match op with
  | Tacky.Complement ->
     [ X86.Mov { src = val_to_reg src; dst = val_to_reg dst }
     ; X86.Not (val_to_reg dst)
     ]
  | Tacky.Negate ->
     [ X86.Mov { src = val_to_reg src; dst = val_to_reg dst }
     ; X86.Neg (val_to_reg dst)
     ]
  | Tacky.Not ->
     [ X86.Cmp (X86.Imm 0, val_to_reg src)
     ; X86.Mov { src = X86.Imm 0; dst = val_to_reg dst }
     ; X86.Sete (val_to_reg dst)
     ]

and comp_binary op src1 src2 dst =
  match op with
  | Tacky.Add ->
     [ X86.Mov { src = val_to_reg src1; dst = val_to_reg dst }
     ; X86.Add { value = val_to_reg src2; dst = val_to_reg dst }
     ]
  | Tacky.Sub ->
     [ X86.Mov { src = val_to_reg src1; dst = val_to_reg dst }
     ; X86.Sub { value = val_to_reg src2; dst = val_to_reg dst }
     ]
  | Tacky.Mult ->
     [ X86.Mov { src = val_to_reg src1; dst = val_to_reg dst }
     ; X86.Imul { value = val_to_reg src2; dst = val_to_reg dst }
     ]
  | Tacky.Div ->
     [ X86.Mov { src = val_to_reg src1; dst = X86.(Reg RAX) }
     ; X86.Cqo
     ; X86.Idiv (val_to_reg src2)
     ; X86.Mov { src = X86.(Reg RAX); dst = val_to_reg dst }
     ]
  | Tacky.Rem ->
     [ X86.Mov { src = val_to_reg src1; dst = X86.(Reg RAX) }
     ; X86.Cqo
     ; X86.Idiv (val_to_reg src2)
     ; X86.Mov { src = X86.(Reg RDX); dst = val_to_reg dst }
     ]
  | Tacky.Equal | Tacky.NotEqual | Tacky.LessThan | Tacky.LessEqual
    | Tacky.GreaterThan | Tacky.GreaterEqual ->
     [ X86.Cmp (val_to_reg src2, val_to_reg src1)
     ; X86.Mov { src = X86.Imm 0; dst = val_to_reg dst }
     ; match op with
       | Tacky.Equal -> X86.Sete (val_to_reg dst)
       | Tacky.NotEqual -> X86.Setne (val_to_reg dst)
       | Tacky.LessThan -> X86.Setl (val_to_reg dst)
       | Tacky.LessEqual -> X86.Setle (val_to_reg dst)
       | Tacky.GreaterThan -> X86.Setg (val_to_reg dst)
       | Tacky.GreaterEqual -> X86.Setge (val_to_reg dst)
       | _ -> failwith "unreachable"
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
  let instr' = List.map (X86.map_operand replace) instr in
  !arg, instr'

and fix_instr instr =
  let apply = function
    | X86.Cmp (X86.Stack s, X86.Stack d) ->
       [ X86.Mov { src = X86.Stack s; dst = X86.Reg X86.R10 }
       ; X86.Cmp (X86.Reg X86.R10, X86.Stack d)
       ]
    | X86.Cmp (op, X86.Imm i) ->
       [ X86.Mov { src = X86.Imm i; dst = X86.Reg X86.R11 }
       ; X86.Cmp (op, X86.Reg X86.R11)
       ]
    | X86.Mov { src = X86.Stack s; dst = X86.Stack d } ->
       [ X86.Mov { src = X86.Stack s; dst = X86.Reg X86.R10 }
       ; X86.Mov { src = X86.Reg X86.R10; dst = X86.Stack d }
       ]
    | X86.Add { value = X86.Stack s; dst = X86.Stack d } ->
       [ X86.Mov { src = X86.Stack s; dst = X86.Reg X86.R10 }
       ; X86.Add { value = X86.Reg X86.R10; dst = X86.Stack d }
       ]
    | X86.Sub { value = X86.Stack s; dst = X86.Stack d } ->
       [ X86.Mov { src = X86.Stack s; dst = X86.Reg X86.R10 }
       ; X86.Sub { value = X86.Reg X86.R10; dst = X86.Stack d }
       ]
    | X86.Imul { value; dst = X86.Stack _ as addr } ->
       [ X86.Mov { src = addr; dst = X86.Reg X86.R11 }
       ; X86.Imul { value; dst = X86.Reg X86.R11 }
       ; X86.Mov { src = X86.Reg X86.R11; dst = addr }
       ]
    | X86.Idiv (X86.Imm i) ->
       [ X86.Mov { src = X86.Imm i; dst = X86.Reg X86.R10 }
       ; X86.Idiv X86.(Reg R10)
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
