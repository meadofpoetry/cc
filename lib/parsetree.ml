
type 'a t = { value : 'a
            ; start : int
            ; _end  : int
            }

type program = PProgram of fun_definition
[@@deriving show]

and fun_definition = PFunction of { name : Id.t; body : block }

and block = PBlock of block_item list

and block_item = PS of statement
               | PD of var_decl

and var_decl = PVar_decl of Id.t * expr option

and statement = PReturn of expr
              | PIf of { cond : expr; _then : statement; _else : statement option }
              | PExpr of expr
              | PCompound of block
              | PBreak of Id.t option
              | PContinue of Id.t option
              | PWhile of { cond : expr; body : statement; loop_id : Id.t option }
              | PDoWhile of { body : statement; cond : expr; loop_id : Id.t option }
              | PFor of { init : for_init option
                        ; cond : expr option
                        ; post : expr option
                        ; body : statement
                        ; loop_id : Id.t option
                        }
              | PNull

and for_init = PInitDecl of var_decl
             | PInitExpr of expr

and expr = PConst of int
         | PVar of Id.t
         | PAssign of expr * expr
         | PUn_op of un_op * expr
         | PBin_op of bin_op * expr * expr
         | PTernary of { cond : expr; _then : expr; _else : expr }

and un_op = PNeg
          | PBit_neg
          | PNot

and bin_op = PAdd
           | PSub
           | PMult
           | PDiv
           | PRem
           | PAnd
           | POr
           | PEqual
           | PNotEqual
           | PLessThan
           | PLessEqual
           | PGreaterThan
           | PGreaterEqual
