open MinCaml
open BacCaml
open Stdlib

type backend =
   | Intel
   | ARM
   | Virtual

let backend_type = ref ARM
let debug = ref false
let ast_dump = ref false
let with_flag flag ~tru:f ~fls:g = if !flag then f () else g ()

let ast oc l =
  Id.counter := 0;
  Parser.exp Lexer.token l |> Syntax.show |> print_endline
;;

let lexbuf oc l =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Parser.exp Lexer.token l
  |> Typing.f
  |> KNormal.f
  |> Alpha.f
  |> Util.(iter !limit)
  |> Closure.f
  |> Virtual.f
  |> Simm.f
  |> fun p ->
  match !backend_type with
  | Intel -> X64.RegAlloc.f p |> X64.Emit.f oc
  | ARM -> Arm64.RegAlloc.f p |> Arm64.Emit.f oc
  | Virtual -> Stdlib.output_bytes oc (Marshal.to_bytes (Emit.f p) [Marshal.No_sharing])

let string s = lexbuf stdout (Lexing.from_string s)

let open_rewrite f = open_out_gen [Open_binary;Open_wronly;Open_creat] 0o644 f 


let backend_type_to_arch = function
  | Intel   -> "x64"
  | ARM     -> "arm64"
  | Virtual -> failwith "Virtual machine does not have an architecture"

let backend_type_to_suffix = function
  | Intel   -> ".intel.s"
  | ARM     -> ".arm.s"
  | Virtual -> ".joe"


let write_file f f_without_filename sufix =
  let inchan = open_in f in    
  let outchan = open_rewrite (f_without_filename ^ sufix) in
  try
    let input = Lexing.from_channel inchan in
    with_flag ast_dump
      ~tru:(fun _ -> ast outchan input)
      ~fls:(fun _ -> lexbuf outchan input; close_in inchan; close_out outchan);
    with
  | e ->
    close_in inchan;
    close_out outchan;
    raise e

let main f =
  let f_without_filename = Filename.remove_extension f in
  let sufix = backend_type_to_suffix !backend_type in
  write_file f f_without_filename sufix;
  match !backend_type with
    | Intel | ARM ->
      ( let arch = backend_type_to_arch !backend_type in
        String.concat " " [ "gcc"; f_without_filename^sufix;
                            "src/"^arch^"/libmincaml.c src/"^arch^"/stub.c -o ";
                            f_without_filename ^ ".exe"] |> Sys.command |> ignore )
    | _ -> ()

let () =
  let files = ref [] in
  Arg.parse
    [ ( "-inline",  Arg.Int  (fun i -> Inline.threshold := i) , "maximum size of functions inlined" ) ;
      ( "-iter",    Arg.Int  (fun i -> Util.limit := i), "maximum number of optimizations iterated" ) ;
      ( "-ast",     Arg.Unit (fun _ -> ast_dump := true), "emit abstract syntax tree" ) ;
      ( "-x86",     Arg.Unit (fun _ -> backend_type := Intel) , "emit IA32 machine code" ) ;
      ( "-x64",     Arg.Unit (fun _ -> backend_type := Intel) , "emit EM64T machine code" ) ;
      ( "-arm" ,    Arg.Unit (fun _ -> backend_type := ARM) , "emit AArch64 machine code" ) ;
      ( "-vm" ,     Arg.Unit (fun _ -> backend_type := Virtual) , "emit MinCaml IR virtual machine" ) ;
      ( "-debug",   Arg.Unit (fun _ -> debug := true), "enable debug mode" )
    ]
    (fun s -> files := !files @ [ s ])
    ("MinCaml EM64T/AArch64/IR Compiler (c) 2024 Namdak Tonpa\n"
    ^ "usage: joe [-inline m] [-iter n] ... filenames" );
  List.iter (fun f -> main f) !files

