
type program = PProgram of fun_decl
[@@deriving show]

and fun_decl = PFunction of { name : Id.t; body : statement }

and statement = PReturn of expr

and expr = PConst of int
