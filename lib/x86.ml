
type program = Program of fun_decl

and fun_decl = Function of { name : Id.t; instr : instr list }

and instr = Mov of { src : operand; dst : operand }
          | Ret

and operand = Imm of int
            | Register

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
     Format.fprintf out "\tmovl\t%a, %a" pp_operand src pp_operand dst
  | Ret ->
     Format.pp_print_string out "\tret"

and pp_operand out = function
  | Imm i ->
     Format.fprintf out "$%d" i
  | Register ->
     Format.pp_print_string out "%eax"
