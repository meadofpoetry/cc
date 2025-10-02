
type t = { token : token
         ; start : int
         ; _end  : int
         } [@@deriving show]

and token =
  | TId of string
  | TNumber of int
  (* C Keywords *)
  | TInt
  | TVoid
  | TReturn
  | TLPar
  | TRPar
  | TLBrace
  | TRBrace
  | TSemicol

exception Lexer_failure of { ch : char; pos : int }

module State = struct

  type t = {
      in_channel    : in_channel;
      mutable pos   : int;
      mutable ch    : char;
      mutable empty : bool;
    }

  let next t =
    if t.empty then None
    else try
      t.ch <- input_char t.in_channel;
      t.pos <- t.pos + 1;
      Some t.ch
    with End_of_file ->
      t.empty <- true;
      None

  let pos t = t.pos
  
  let peek t = t.ch

  let is_end t = t.empty
  
  let make in_channel : t =
    let res = { in_channel = in_channel;
                ch = ' ';
                pos = -1;
                empty = false
              }
    in
    ignore @@ next res;
    res
end

let is_letter ch =
  match ch with
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

let is_digit ch =
  match ch with
  | '0'..'9' -> true
  | _ -> false

let is_blank ch =
  match ch with
  | '\t' | ' ' | '\n' -> true
  | _ -> false

let is_symbol ch =
  match ch with
  | '(' | ')' | '{' | '}' | ';' | ',' -> true
  | _ -> false

let rec tokens in_channel : < token : t option > =
  let state = State.make in_channel in
  object
    method token : t option = next_token state ()
  end

and next_token state () =
  if State.is_end state then None
  else match State.peek state with
  | '(' ->
     let start = State.pos state in
     ignore @@ State.next state;
     let _end  = State.pos state in
     Some { token = TLPar; start = start; _end = _end }
  | ')' ->
     let start = State.pos state in
     ignore @@ State.next state;
     let _end  = State.pos state in
     Some { token = TRPar; start = start; _end = _end }
  | '{' ->
     let start = State.pos state in
     ignore @@ State.next state;
     let _end  = State.pos state in
     Some { token = TLBrace; start = start; _end = _end }
  | '}' ->
     let start = State.pos state in
     ignore @@ State.next state;
     let _end  = State.pos state in
     Some { token = TRBrace; start = start; _end = _end }
  | ';' ->
     let start = State.pos state in
     ignore @@ State.next state;
     let _end  = State.pos state in
     Some { token = TSemicol; start = start; _end = _end }
  | c when is_letter c ->
     identifier state c
  | d when is_digit d ->
     number state d
  | ' ' | '\t' | '\n' ->
     ignore @@ State.next state;
     next_token state ()
  | '/' ->
     ignore @@ State.next state;
     skip_comment state;
     next_token state ()
  | c ->
     raise (Lexer_failure { ch = c; pos = State.pos state })

and identifier state ch =
  let start = State.pos state in
  let sb = Buffer.create 16 in
  let ch = ref ch in
  while not (State.is_end state) && (is_letter !ch || is_digit !ch || !ch = '_') do
    Buffer.add_char sb !ch;
    ignore @@ State.next state;
    ch := State.peek state
  done;
  let _end = State.pos state in
  let string = Buffer.contents sb in
  begin match string with
  | "void" ->
     Some { token = TVoid; start = start; _end = _end }
  | "int" ->
     Some { token = TInt; start = start; _end = _end }
  | "return" ->
     Some { token = TReturn; start = start; _end = _end }
  | _ ->
     Some { token = TId string; start = start; _end = _end }
  end

and number state ch =
  let start = State.pos state in
  let sb = Buffer.create 16 in

  let rec loop = function
    | ch when is_blank ch || is_symbol ch ->
       ()
    | ch when is_digit ch ->
       Buffer.add_char sb ch;
       begin match (State.next state) with
       | None -> ()
       | Some ch -> loop ch
       end
    | ch ->
       raise (Lexer_failure { ch = ch; pos = State.pos state })
  in
  loop ch;
  let _end = State.pos state in
  let value = int_of_string @@ Buffer.contents sb in
  Some { token = TNumber value; start = start; _end = _end }

and skip_comment state =
  match State.peek state with
  (* Multi-line comment *)
  | '*' ->
     let rec loop prev =
       match State.next state with
       | Some '/' when prev = '*' ->
          ignore @@ State.next state;
          ()
       | Some c -> loop c
       | None -> ()
     in loop '*'
  (* Single-line comment *)
  | '/' ->
     let rec loop () =
       match State.next state with
       | Some '\n' ->
          ignore @@ State.next state;
          ()
       | Some _ -> loop ()
       | None -> ()
     in loop ()
  | _ -> failwith "unreachable"
