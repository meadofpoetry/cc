
type program = Program of fun_decl

and fun_decl = Function of { name : Id.t; instr : instr list }

and instr = Mov of { src : operand; dst : operand }
          | Neg of operand
          | Not of operand
          | Add of { value : operand; dst : operand }
          | Sub of { value : operand; dst : operand }
          | Imul of { value : operand; dst : operand }
          | Idiv of operand
          | Cqo
          | Cmp of operand * operand
          (* Jump instructions *)
          | Jmp of Id.t
          | Je of Id.t
          | Jne of Id.t
          | Jg of Id.t
          | Jge of Id.t
          | Jl of Id.t
          | Jle of Id.t
          (* Cond set *)
          | Sete of operand
          | Setne of operand
          | Setl of operand
          | Setle of operand
          | Setg of operand
          | Setge of operand
          (* Stack *)
          | Push of operand
          | Pop of operand
          | Ret
          (* Jump label *)
          | Label of Id.t

and operand = Imm of int
            | Reg of reg
            | Pseudo of Id.t
            | Stack of int

and reg = RAX | RDX | RBP | RSP | R10 | R11

let map_operand f = function
  | Mov { src; dst } -> Mov { src = f src; dst = f dst }
  | Neg op -> Neg (f op)
  | Not op -> Not (f op)
  | Add { value; dst } -> Add { value = f value; dst = f dst }
  | Sub { value; dst } -> Sub { value = f value; dst = f dst }
  | Imul { value; dst } -> Imul { value = f value; dst = f dst }
  | Idiv op -> Idiv (f op)
  | Cqo -> Cqo
  | Cmp (op1, op2) -> Cmp (f op1, f op2)
  | Jmp _ as instr -> instr
  | Je _ as instr -> instr
  | Jne _ as instr -> instr
  | Jl _ as instr -> instr
  | Jle _ as instr -> instr
  | Jg _ as instr -> instr
  | Jge _ as instr -> instr
  | Sete op -> Sete (f op)
  | Setne op -> Setne (f op)
  | Setl op -> Setl (f op)
  | Setle op -> Setle (f op)
  | Setg op -> Setg (f op)
  | Setge op -> Setge (f op)
  | Push op -> Push (f op)
  | Pop op -> Pop (f op)
  | Ret -> Ret
  | Label _ as instr -> instr

let prolog argc =
  [ Push (Reg RBP)
  ; Mov  { src = Reg RSP; dst = Reg RBP }
  ; Sub  { value = Imm (argc * 8); dst = Reg RSP }
  ]

let epilog =
  [ Mov  { src = Reg RBP; dst = Reg RSP }
  ; Pop (Reg RBP)
  ]

let rec pp_program out (Program f) =
  let open Format in
  pp_print_string out "\t.section .note.GNU-stack,\"\",@progbits\n";
  pp_print_string out "\t.text\n";
  pp_fun_decl out f;
  pp_print_flush out ();

and pp_fun_decl out (Function { name; instr }) =
  let open Format in
  fprintf out "\t.globl %s" name;
  pp_force_newline out ();
  fprintf out "%s:" name;
  pp_force_newline out ();
  pp_print_list ~pp_sep:pp_force_newline pp_instr out instr;
  pp_force_newline out ()

and pp_instr out = function
  | Mov { src; dst } ->
     emit out "movq" [src; dst]
  | Add { value; dst } ->
     emit out "addq" [value; dst]
  | Sub { value; dst } ->
     emit out "subq" [value; dst]
  | Imul { value; dst } ->
     emit out "imulq" [value; dst]
  | Idiv value ->
     emit out "idivq" [value]
  | Cqo ->
     emit out "cqo" []
  | Cmp (op1, op2) ->
     emit out "cmpq" [op1; op2]
  | Jmp label ->
     emit_jmp out "jmp" label
  | Je label ->
     emit_jmp out "je" label
  | Jne label ->
     emit_jmp out "jne" label
  | Jl label ->
     emit_jmp out "jl" label
  | Jle label ->
     emit_jmp out "jle" label
  | Jg label ->
     emit_jmp out "jg" label
  | Jge label ->
     emit_jmp out "jge" label
  | Sete op ->
     emit out "sete" [op]
  | Setne op ->
     emit out "setne" [op]
  | Setl op ->
     emit out "setl" [op]
  | Setle op ->
     emit out "setle" [op]
  | Setg op ->
     emit out "setg" [op]
  | Setge op ->
     emit out "setge" [op]
  | Push op ->
     emit out "pushq" [op]
  | Pop op ->
     emit out "popq" [op]
  | Neg op ->
     emit out "negq" [op]
  | Not op ->
     emit out "notq" [op]
  | Ret ->
     emit out "ret" []
  | Label label ->
     emit_label out label

and pp_operand out = function
  | Imm i ->
     Format.fprintf out "$%d" i
  | Reg RAX ->
     Format.pp_print_string out "%rax"
  | Reg RDX ->
     Format.pp_print_string out "%rdx"
  | Reg RBP ->
     Format.pp_print_string out "%rbp"
  | Reg RSP ->
     Format.pp_print_string out "%rsp"
  | Reg R10 ->
     Format.pp_print_string out "%r10"
  | Reg R11 ->
     Format.pp_print_string out "%r11"
  | Stack off ->
     Format.fprintf out "-%d(%%rbp)" (off * 8)
  | Pseudo name ->
     Format.fprintf out "P{%s}" name

and emit out instr args =
  let open Format in
  fprintf out "\t%s\t" instr;
  pp_print_list ~pp_sep:(fun out () -> pp_print_string out ", ") pp_operand out args

and emit_jmp out instr label =
  let open Format in
  fprintf out "\t%s\t.L%s" instr label

and emit_label out label =
  let open Format in
  fprintf out ".L%s:" label
