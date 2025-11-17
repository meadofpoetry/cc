
type program = top_level list [@@deriving show]

and top_level = Function of { name : Id.t; params : Id.t list; body : instr list; global : bool }
              | StaticVar of { name : Id.t; init : int; global : bool }

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
