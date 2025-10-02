open Parsetree

let rec compile : Parsetree.program -> X86.program =
  fun (PProgram f) -> X86.Program (comp_fun_decl f)

and comp_fun_decl (PFunction { name; body }) =
  let instr = comp_statement body in
  X86.Function { name = name; instr = instr }

and comp_statement = function
  | PReturn (PConst i) ->
     [ X86.Mov { src = X86.Imm i; dst = X86.Register }
     ; X86.Ret
     ]
