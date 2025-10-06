
type program = PProgram of fun_decl
[@@deriving show]

and fun_decl = PFunction of { name : Id.t; body : statement }

and statement = PReturn of expr

and expr = PConst of int
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
