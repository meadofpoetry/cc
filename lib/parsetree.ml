
type program = PProgram of fun_definition
[@@deriving show]

and fun_definition = PFunction of { name : Id.t; body : block_item list }

and block_item = PS of statement
               | PD of var_decl

and var_decl = PVar_decl of Id.t * expr option

and statement = PReturn of expr
              | PExpr of expr
              | PNull

and expr = PConst of int
         | PVar of Id.t
         | PAssign of expr * expr
         | PUn_op of un_op * expr
         | PBin_op of bin_op * expr * expr

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
