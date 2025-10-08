[@@@warning "-partial-match"]

exception Unexpected_eof

exception Unexpected_token of { msg : string; actual : Lexer.t }

let unexpected_token ~actual (fmt : ('a, Format.formatter, unit, 'nether) format4) : 'a =
  Format.kasprintf (fun msg -> raise (Unexpected_token { msg = msg; actual = actual })) fmt

class stream (t : < token : Lexer.t option >) =
  let open Lexer in
  let tokens = t in
  object(self)
    val mutable state = tokens#token

    method current =
      match state with
      | None -> raise Unexpected_eof
      | Some s -> s

    method next =
      let cur = self#current in
      state <- tokens#token;
      cur

    method skip =
      ignore @@ self#next
    
    method accept tok =
      if self#current.token = tok
      then ignore @@ self#skip
      else unexpected_token ~actual:self#current "Expected token %a" Lexer.pp_token tok
  end

[@@warning "-partial-match"]
let token_value stream f ~err_msg =
  try
    let res = f (stream#current.Lexer.token) in
    stream#skip;
    res
  with Match_failure _ -> unexpected_token ~actual:stream#current err_msg

let is_binop = function
  | Lexer.TPlus | Lexer.TMinus | Lexer.TStar | Lexer.TSlash | Lexer.TPercent
    | Lexer.TLeq | Lexer.TLt | Lexer.TGt | Lexer.TGeq
    | Lexer.TEqEq | Lexer.TExclamEq | Lexer.TEq
    |  Lexer.TAmpAmp | Lexer.TBarBar -> true
  | _ -> false

let precedence = function
  | Lexer.TStar | Lexer.TSlash | Lexer.TPercent -> 50 
  | Lexer.TPlus | TMinus -> 45
  | Lexer.TLeq | Lexer.TLt | Lexer.TGt | Lexer.TGeq -> 35
  | Lexer.TEqEq | Lexer.TExclamEq -> 30
  | Lexer.TAmpAmp -> 10
  | Lexer.TBarBar -> 5
  | Lexer.TEq -> 1
  | tok -> failwith ("Unexpected precedence token: " ^ Lexer.show_token tok)

let rec parse (tokens : < token : Lexer.t option >) : Parsetree.program =
  let s = new stream tokens in
  let res = parse_program s in
  ensure_end s;
  res

and parse_program stream =
  Parsetree.PProgram(parse_fun stream)

and parse_fun stream =
  stream#accept TInt;
  let name = token_value stream
               (function (Lexer.TId n) -> n)
               ~err_msg:"Expected function name"
  in
  stream#accept TLPar;
  stream#accept TVoid;
  stream#accept TRPar;
  stream#accept TLBrace;
  let items = parse_block_items [] stream in
  stream#accept TRBrace;
  Parsetree.PFunction {
      name = name;
      body = items
    }

and parse_block_items acc stream =
  let next_tok = stream#current in
    if next_tok.token == TRBrace
    then List.rev acc
    else
      let item = parse_block_item stream in
      parse_block_items (item::acc) stream

and parse_block_item stream =
  let next_tok = stream#current in
  if next_tok.token == TInt
  then PD (parse_var_decl stream)
  else PS (parse_statement stream)

and parse_var_decl stream =
  stream#accept TInt;
  let name = token_value stream
               (function (Lexer.TId n) -> n)
               ~err_msg:"Expected variable name"
  in
  if stream#current.token != TEq
  then Parsetree.PVar_decl (name, None)
  else begin
      stream#accept TEq;
      let expr = parse_expr stream in
      Parsetree.PVar_decl (name, Some expr)
    end

and parse_statement stream =
  let next_tok = stream#current in
  match next_tok.token with
  | TReturn ->
     stream#skip;
     let expr = parse_expr stream in
     stream#accept TSemicol;
     Parsetree.PReturn expr
  | TSemicol ->
     stream#skip;
     Parsetree.PNull
  | _ ->
     let expr = parse_expr stream in
     stream#accept TSemicol;
     Parsetree.PExpr expr

and parse_expr stream =
  parse_expr_prec stream 0

and parse_expr_prec stream min_prec =
  let rec loop expr =
    let next_tok = stream#current in
    if is_binop next_tok.token && precedence next_tok.token > min_prec
    then
      if next_tok.token == TEq
      then
        let _ = stream#skip in
        (* = is right associative so min precedence is < prec(=) to parse a = (b = c) *)
        let right = parse_expr_prec stream (precedence next_tok.token - 1) in
        loop (Parsetree.PAssign (expr, right))
      else
        let op = parse_binop stream in
        let right = parse_expr_prec stream (precedence next_tok.token) in
        loop (Parsetree.PBin_op (op, expr, right))
    else
      expr
  in
  let left = parse_factor stream in
  loop left

and parse_factor stream =
  let next_tok = stream#current in
  match next_tok.token with
  | Lexer.TNumber i ->
     stream#skip;
     Parsetree.PConst i
  | Lexer.TId name ->
     stream#skip;
     Parsetree.PVar name
  | Lexer.TTilde | Lexer.TMinus | Lexer.TExclam -> (* ~-! *)
     let op   = parse_unop stream in
     let exp' = parse_factor stream in
     Parsetree.PUn_op (op, exp')
  | Lexer.TLPar ->
     stream#skip;
     let exp' = parse_expr stream in
     stream#accept Lexer.TRPar;
     exp'
  | _ -> unexpected_token "Expression:" ~actual:next_tok

and parse_unop stream =
  let next_tok = stream#next in
  match next_tok.token with
  | Lexer.TTilde -> Parsetree.PBit_neg
  | Lexer.TMinus -> Parsetree.PNeg
  | Lexer.TExclam -> Parsetree.PNot
  | _ -> unexpected_token "Unary:" ~actual:next_tok

and parse_binop stream =
  let next_tok = stream#next in
  match next_tok.token with
  | Lexer.TPlus -> Parsetree.PAdd
  | Lexer.TMinus -> Parsetree.PSub
  | Lexer.TStar -> Parsetree.PMult
  | Lexer.TSlash -> Parsetree.PDiv
  | Lexer.TPercent -> Parsetree.PRem
  | Lexer.TAmpAmp -> Parsetree.PAnd
  | Lexer.TBarBar -> Parsetree.POr
  | Lexer.TEqEq -> Parsetree.PEqual
  | Lexer.TExclamEq -> Parsetree.PNotEqual
  | Lexer.TLt -> Parsetree.PLessThan
  | Lexer.TLeq -> Parsetree.PLessEqual
  | Lexer.TGt -> Parsetree.PGreaterThan
  | Lexer.TGeq -> Parsetree.PGreaterEqual
  | _ -> unexpected_token "Binary:" ~actual:next_tok

and ensure_end stream =
  try
    let tok = stream#current in
    raise (Unexpected_token { msg = "Expected end of program here"; actual = tok })
  with
  | Unexpected_eof -> ()
