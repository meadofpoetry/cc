
type program = Program of fun_decl [@@deriving show]

and fun_decl = Function of { name : Id.t; body : instr list }

and instr = Return of value
          | Unary of { op : un_op; src : value; dst : value }

and value = Const of int
          | Var of Id.t

and un_op = Complement
          | Negate
