
type 'a t = { value : 'a
            ; start : int
            ; _end  : int
            }

type program = fun_decl list
[@@deriving show]

and decl = PFun_decl of fun_decl
         | PVar_decl of var_decl

and fun_decl = { name : Id.t
               ; args : Id.t list
               ; body : block option
               }

and var_decl = Id.t * expr option

and block = block_item list

and block_item = PS of statement
               | PD of decl

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
         | PFun_call of Id.t * expr list
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
