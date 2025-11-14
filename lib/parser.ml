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

    method is_eof =
      Option.is_none state
    
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
    |  Lexer.TAmpAmp | Lexer.TBarBar | Lexer.TQuestion -> true
  | _ -> false

let is_specifier = function
  | Lexer.TStatic | Lexer.TExtern -> true
  | Lexer.TInt -> true
  | _ -> false

let precedence = function
  | Lexer.TStar | Lexer.TSlash | Lexer.TPercent -> 50 
  | Lexer.TPlus | TMinus -> 45
  | Lexer.TLeq | Lexer.TLt | Lexer.TGt | Lexer.TGeq -> 35
  | Lexer.TEqEq | Lexer.TExclamEq -> 30
  | Lexer.TAmpAmp -> 10
  | Lexer.TBarBar -> 5
  | Lexer.TQuestion -> 3
  | Lexer.TEq -> 1
  | tok -> failwith ("Unexpected precedence token: " ^ Lexer.show_token tok)

let optional (type a) ~(parser : stream -> a) ~(next_token : Lexer.token) (stream : stream) : a option =
  if stream#current.token = next_token then None
  else begin
      let res = parser stream in
      if stream#current.token <> next_token
      then unexpected_token ~actual:stream#current "Optional expression: bad ending token";
      Some res
    end

let list (type a) ~(parser : stream -> a option) (stream : stream) : a list =
  let rec loop acc =
    match parser stream with
    | None -> List.rev acc
    | Some v -> loop (v :: acc)
  in
  loop []

let rec parse (tokens : < token : Lexer.t option >) : Parsetree.program =
  let s = new stream tokens in
  let res = parse_program s in
  ensure_end s;
  res

and parse_program stream =
  let rec loop fs =
    if stream#is_eof
    then List.rev fs
    else loop (parse_decl stream :: fs)
  in
  loop []

and parse_specifier (stream : stream) =
  if is_specifier stream#current.token
  then Some stream#next.token
  else None

and parse_decl stream =
  let specifiers = list ~parser:parse_specifier stream in
  let name = token_value stream
               (function (Lexer.TId n) -> n)
               ~err_msg:"Expected name"
  in
  if stream#current.token == TLPar
  then Parsetree.PFun_decl (parse_fun_decl_rest stream specifiers name)
  else Parsetree.PVar_decl (parse_var_decl_rest stream specifiers name)

and parse_fun_decl stream =
  let specifiers = list ~parser:parse_specifier stream in
  let name = token_value stream
               (function (Lexer.TId n) -> n)
               ~err_msg:"Expected function name"
  in
  parse_fun_decl_rest stream specifiers name

and parse_fun_decl_rest stream specifiers name =
  let open Lexer in
  let storage_class = match specifiers with
    | [ TStatic; TInt ] | [ TInt; TStatic ] -> Some Parsetree.PStatic
    | [ TExtern; TInt ] | [ TInt; TExtern ] -> Some Parsetree.PExtern
    | [ TInt ] -> None
    | l -> failwith (Format.asprintf "Unexpected function specifiers: %a\n" (Format.pp_print_list Lexer.pp_token) l)
  in
  stream#accept TLPar;
  let args = parse_param_list stream in
  stream#accept TRPar;
  let block =
    if stream#current.token == TSemicol
    then begin stream#accept TSemicol; None end
    else Some (parse_block stream)
  in
  { name = name
  ; args = args
  ; body = block
  ; storage_class = storage_class
  }

and parse_var_decl stream =
  let specifiers = list ~parser:parse_specifier stream in
  let name = token_value stream
               (function (Lexer.TId n) -> n)
               ~err_msg:"Expected variable name"
  in
  parse_var_decl_rest stream specifiers name

and parse_var_decl_rest stream specifiers name =
  let storage_class = match specifiers with
    | [ TStatic; TInt ] | [ TInt; TStatic ] -> Some Parsetree.PStatic
    | [ TExtern; TInt ] | [ TInt; TExtern ] -> Some Parsetree.PExtern
    | [ TInt ] -> None
    | l -> failwith (Format.asprintf "Unexpected var specifiers: %a\n" (Format.pp_print_list Lexer.pp_token) l)
  in
  let init =
    if stream#current.token != TEq then None
    else begin
        stream#accept TEq;
        Some (parse_expr stream)
      end
  in
  stream#accept TSemicol;
  { name; init; storage_class }

and parse_param_list stream =
  let rec loop args =
    stream#accept Lexer.TInt;
    let name = token_value stream
                 (function (Lexer.TId n) -> n)
                 ~err_msg:"Expected variable name"
    in
    if stream#current.token = TComma
    then begin stream#accept TComma; loop (name :: args) end
    else List.rev (name :: args)
  in
  if stream#current.token == TVoid
  then begin stream#accept TVoid; [] end
  else loop []

and parse_block stream =
  let rec parse_items acc =
    let next_tok = stream#current in
    if next_tok.token == TRBrace
    then List.rev acc
    else
      let item = parse_block_item stream in
      parse_items (item::acc)
  in
  stream#accept TLBrace;
  let items = parse_items [] in
  stream#accept TRBrace;
  items

and parse_block_item stream =
  let next_tok = stream#current in
  if is_specifier next_tok.token
  then PD (parse_decl stream)
  else PS (parse_statement stream)

and parse_statement stream =
  let next_tok = stream#current in
  match next_tok.token with
  | TReturn ->
     stream#skip;
     let expr = parse_expr stream in
     stream#accept TSemicol;
     Parsetree.PReturn expr
  | TBreak ->
     stream#skip;
     stream#accept TSemicol;
     Parsetree.PBreak None
  | TContinue ->
     stream#skip;
     stream#accept TSemicol;
     Parsetree.PContinue None
  | TIf ->
     stream#skip;
     stream#accept TLPar;
     let cond = parse_expr stream in
     stream#accept TRPar;
     let _then = parse_statement stream in
     let _else =
       if stream#current.token != TElse then None          
       else
         let () = stream#skip in
         Some (parse_statement stream)
     in
     Parsetree.PIf { cond; _then; _else }
  | TWhile ->
     stream#skip;
     stream#accept TLPar;
     let cond = parse_expr stream in
     stream#accept TRPar;
     let body = parse_statement stream in
     Parsetree.PWhile { cond; body; loop_id = None }
  | TDo ->
     stream#skip;
     let body = parse_statement stream in
     stream#accept TWhile;
     stream#accept TLPar;
     let cond = parse_expr stream in
     stream#accept TRPar;
     stream#accept TSemicol;
     Parsetree.PDoWhile { body; cond; loop_id = None }
  | TFor ->
     stream#skip;
     stream#accept TLPar;
     let init = parse_for_init stream in
     let cond = optional ~parser:parse_expr ~next_token:TSemicol stream in
     stream#accept TSemicol;
     let post = optional ~parser:parse_expr ~next_token:TRPar stream in
     stream#accept TRPar;
     let body = parse_statement stream in
     Parsetree.PFor { init; cond; post; body; loop_id = None }
  | TLBrace ->
     PCompound (parse_block stream)
  | TSemicol ->
     stream#skip;
     Parsetree.PNull
  | _ ->
     let expr = parse_expr stream in
     stream#accept TSemicol;
     Parsetree.PExpr expr

and parse_for_init stream : Parsetree.for_init option =
  match stream#current.token with
  | TSemicol ->
     stream#skip;
     None
  | tok when is_specifier tok ->
     Some (Parsetree.PInitDecl (parse_var_decl stream))
  | _ ->
     let e = parse_expr stream in
     stream#accept TSemicol;
     Some (Parsetree.PInitExpr e)

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
      else if next_tok.token == TQuestion
      then
        let _ = stream#skip in
        (* ? is right associative *)
        let middle = parse_expr stream in
        stream#accept Lexer.TColon;
        let right = parse_expr_prec stream (precedence next_tok.token - 1) in
        loop (Parsetree.PTernary { cond = expr; _then = middle; _else = right })
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
  (* Var or Fun_call *)
  | Lexer.TId name ->
     stream#skip;
     if stream#current.token = TLPar
     then
       let args = parse_arg_list stream in
       Parsetree.PFun_call (name, args)
     else Parsetree.PVar name
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

and parse_arg_list stream =
  let rec parse_args () =
    if stream#current.token = TRPar
    then begin stream#accept TRPar; [] end
    else
      let exp = parse_expr stream in
      match stream#current.token with
      | TRPar ->
         stream#accept TRPar;
         [exp]
      | TComma ->
         parse_args_rest [exp]
      | _ -> unexpected_token "Arg list:" ~actual:stream#current
  and parse_args_rest args =
    stream#accept TComma;
    let exp = parse_expr stream in
    if stream#current.token = TRPar
    then begin stream#accept TRPar; List.rev (exp::args) end
    else parse_args_rest (exp::args)
  in
  stream#accept TLPar;
  parse_args ()

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
