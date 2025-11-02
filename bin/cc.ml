
exception Asm_error of int
exception Linking_error of int

let emit_lex = ref false
let emit_parsetree = ref false
let validate_only = ref false
let emit_tacky = ref false
let emit_codegen = ref false
let compile_obj = ref false

let filepaths = Dynarray.create ()

let usage = "cc [--lex|--parse|--validate|--tacky|--codegen|-c] file.c [file2.c file3.c]"

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

let assemble filepath asm_file =
  let target = Filename.chop_extension filepath ^ ".o" in
  let command = Printf.sprintf "gcc -c %s -o %s" asm_file target in
  let exit_code = Sys.command command in
  if exit_code != 0
  then raise (Asm_error exit_code);
  if not !compile_obj
  then target
  else raise Exit  

let link target obj_files =
  let command = "gcc " ^ (String.concat " " obj_files) ^ " -o " ^ target in
  let exit_code = Sys.command command in
  if exit_code != 0
  then raise (Linking_error exit_code)

let () =
  let set_filepath arg =
    Dynarray.add_last filepaths arg
  in
  let args =
    [
      ("--lex", Arg.Set emit_lex, "print lexer result and exit");
      ("--parse", Arg.Set emit_parsetree, "print parser result and exit");
      ("--validate", Arg.Set validate_only, "semantic analysis and exit");
      ("--tacky", Arg.Set emit_tacky, "print tacky ir and exit");
      ("--codegen", Arg.Set emit_codegen, "print resulting assembly and exit");
      ("-c", Arg.Set compile_obj, "compile object file and bypass lining step");
    ]
  in
  Arg.parse args set_filepath usage;
  try
    let file_list = Dynarray.to_list filepaths in
    let output_file = Filename.chop_extension @@ List.hd file_list in
    file_list
    |> List.map (fun filepath ->
           let target = Filename.chop_extension @@ filepath in
           let source_file = cpp filepath in
           In_channel.with_open_text source_file
             (fun in_channel ->
               lexer in_channel
               |> parser
               |> validate
               |> ir_gen
               |> codegen
               |> write_out_s
               |> assemble target))
    |> link output_file
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
     | Asm_error exit_code ->
        Format.eprintf "Asm: exit code %d\n" exit_code;
        exit 1
     | Linking_error exit_code ->
        Format.eprintf "Linking: exit code %d\n" exit_code;
        exit 1
     | Failure msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 1
