open Format
open Lexing
open Error

let usage = "usage: compilo [options] file.ml"

let interpret  = ref false
let compile    = ref true

let spec =
  [ "-i", Arg.Tuple [Arg.Set interpret; Arg.Clear compile],
    "  interpreter only";
  ]

let file = 
  let file = ref None in
  let set_file s =  
    if not (Filename.check_suffix s ".ml") then 
      raise (Arg.Bad "no .ml extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let p = Parser.prog Lexer.token lb in
    close_in c;
    if !compile
    then begin
      let prog = Compile.compile_prog p in
      let output_file = (Filename.chop_suffix file ".ml") ^ ".asm" in
      let out = open_out output_file in
      let outf = formatter_of_out_channel out in
      Mips.print_program outf prog;
      pp_print_flush outf ();
      close_out out
    end
    else ();
    if !interpret
    then Interpreter.interpret_prog p
    else ();
    exit 0
  with
    | Error.Error (e,p) ->
      Error.print err_formatter file e p;
      exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
