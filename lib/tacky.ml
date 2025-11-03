
type program = fun_decl list [@@deriving show]

and fun_decl = { name : Id.t; params : Id.t list; body : instr list }

and instr = Fun_call of { name : Id.t; args : value list; dst : value }
          | Return of value
          | Unary  of { op : un_op; src : value; dst : value }
          | Binary of { op : bin_op; src1 : value; src2 : value; dst : value }
          | Copy of { src : value; dst : value }
          | Label of Id.t
          | Jump of Id.t
          | JumpIfZero of { cond : value; target : Id.t }
          | JumpIfNotZero of { cond : value; target : Id.t }

and value = Const of int
          | Var of Id.t

and un_op = Complement
          | Negate
          | Not

and bin_op = Add
           | Sub
           | Mult
           | Div
           | Rem
           | Equal
           | NotEqual
           | LessThan
           | LessEqual
           | GreaterThan
           | GreaterEqual
