
type program = Program of fun_decl

and fun_decl = Function of { name : Id.t; instr : instr list }

and instr = Mov of { src : operand; dst : operand }
          | Neg of operand
          | Not of operand
          | Sub of { value : operand; dst : operand }
          | Push of operand
          | Pop of operand
          | Ret

and operand = Imm of int
            | Reg of reg
            | Pseudo of Id.t
            | Stack of int

and reg = RAX | RBP | RSP | R10

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
  | Sub { value; dst } ->
     emit out "subq"  [value; dst]
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

and pp_operand out = function
  | Imm i ->
     Format.fprintf out "$%d" i
  | Reg RAX ->
     Format.pp_print_string out "%rax"
  | Reg RBP ->
     Format.pp_print_string out "%rbp"
  | Reg RSP ->
     Format.pp_print_string out "%rsp"
  | Reg R10 ->
     Format.pp_print_string out "%r10"
  | Stack off ->
     Format.fprintf out "-%d(%%rbp)" (off * 8)
  | Pseudo name ->
     Format.fprintf out "P{%s}" name

and emit out instr args =
  let open Format in
  fprintf out "\t%s\t" instr;
  pp_print_list ~pp_sep:(fun out () -> pp_print_string out ", ") pp_operand out args
