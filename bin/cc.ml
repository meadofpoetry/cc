
let emit_lex = ref false
let emit_parsetree = ref false
let validate_only = ref false
let emit_tacky = ref false
let emit_codegen = ref false

let filepath = ref ""

let usage = "cc [--lex|--parse|--validate|--tacky|--codegen] file.c"

let cpp file =
  let basename = Filename.(basename file |> chop_extension) in
  let outfile  = Filename.temp_file basename ".pp.c" in
  let command  = Printf.sprintf "cpp -P %s -o %s" file outfile in
  let res = Sys.command command in
  if res != 0
  then failwith "Preprocessor failed"
  else outfile

let lexer in_channel =
  let tokens = Lexer.tokens in_channel in
  if not !emit_lex
  then tokens
  else begin
      let rec loop () =
        match tokens#token with
        | None -> ()
        | Some tok ->
           print_endline @@ Lexer.show tok;
           loop ()
      in
      loop ();
      raise Exit
    end


let parser tokens =
  let parsetree = Parser.parse tokens in
  if not !emit_parsetree
  then parsetree
  else begin
      Format.printf "%a\n" Parsetree.pp_program parsetree;
      raise Exit
    end

let validate parsetree =
  let parsetree' = Semantic.validate parsetree in
  if not !validate_only
  then parsetree'
  else begin
      Format.printf "%a\n" Parsetree.pp_program parsetree';
      raise Exit
    end

let ir_gen parsetree =
  let tacky = Backend.tacky parsetree in
  if not !emit_tacky
  then tacky
  else begin
      Format.printf "%a\n" Tacky.pp_program tacky;
      raise Exit
    end

let codegen tacky =
  let asm = Backend.compile tacky in
  if not !emit_codegen
  then asm
  else begin
      Format.printf "%a\n" X86.pp_program asm;
      raise Exit
    end

let write_out_s asm =
  let out_file, out_chan = Filename.open_temp_file "out" ".S" in
  Fun.protect ~finally:(fun () -> Out_channel.close out_chan)
    (fun () ->
      Format.fprintf
        (Format.formatter_of_out_channel out_chan)
        "%a\n"
        X86.pp_program asm);
  out_file

let assemble asm_file =
  let target = Filename.chop_extension !filepath in
  let command = Printf.sprintf "gcc %s -o %s" asm_file target in
  Sys.command command

let () =
  let set_filepath arg =
    filepath := arg
  in
  let args =
    [
      ("--lex", Arg.Set emit_lex, "print lexer result and exit");
      ("--parse", Arg.Set emit_parsetree, "print parser result and exit");
      ("--validate", Arg.Set validate_only, "semantic analysis and exit");
      ("--tacky", Arg.Set emit_tacky, "print tacky ir and exit");
      ("--codegen", Arg.Set emit_codegen, "print resulting assembly and exit");
    ]
  in
  Arg.parse args set_filepath usage;
  try
    let source_file = cpp !filepath in
    In_channel.with_open_text source_file
      (fun in_channel ->
        lexer in_channel
        |> parser
        |> validate
        |> ir_gen
        |> codegen
        |> write_out_s
        |> assemble
        |> exit)
  with Exit ->
        exit 0
     | Lexer.Lexer_failure { ch; pos } ->
        Printf.eprintf "Lexer: unexpected char %c at pos %d\n" ch pos;
        exit 1
     | Parser.Unexpected_eof ->
        Printf.eprintf "Parser: unexpected end of file\n";
        exit 1
     | Parser.Unexpected_token { msg; actual } ->
        Format.eprintf "Parser: %s, got %a\n" msg Lexer.pp actual;
        exit 1
     | Var_resolution.Invalid_lvalue _ ->
        Format.eprintf "Validate: bad lvalue\n";
        exit 1
     | Var_resolution.Redeclaration v ->
        Format.eprintf "Validate: redeclared variable %s\n" v;
        exit 1
     | Var_resolution.Undeclared v ->
        Format.eprintf "Validate: undeclared variable %s\n" v;
        exit 1
     | Failure msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 1
