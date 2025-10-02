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
      state <- tokens#token
    
    method accept tok =
      if self#current.token = tok
      then self#next
      else unexpected_token ~actual:self#current "Expected token %a" Lexer.pp_token tok
  end

[@@warning "-partial-match"]
let token_value stream f ~err_msg =
  try
    let res = f (stream#current.Lexer.token) in
    stream#next;
    res
  with Match_failure _ -> unexpected_token ~actual:stream#current err_msg

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
  let statement = parse_statement stream in
  stream#accept TRBrace;
  Parsetree.PFunction {
      name = name;
      body = statement
    }

and parse_statement stream =
  stream#accept TReturn;
  let expr = parse_expr stream in
  stream#accept TSemicol;
  Parsetree.PReturn expr

and parse_expr stream =
  let i = token_value
            stream
            (function TNumber i -> i)
            ~err_msg:"Constant expected"
  in
  Parsetree.PConst i

and ensure_end stream =
  try
    let tok = stream#current in
    raise (Unexpected_token { msg = "Expected end of program here"; actual = tok })
  with
  | Unexpected_eof -> ()
