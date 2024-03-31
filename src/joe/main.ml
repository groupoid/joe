open MinCaml

type backend =
  | Intel
  | ARM
  | Virtual

let backend_type = ref Intel
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
  | Intel -> RegAlloc.f p |> X64.Emit.f oc
  | ARM -> RegAlloc.f p |> Arm64.Emit.f oc
  | Virtual -> Asm.show_prog p |> Printf.fprintf oc "%s"
;;

let string s = lexbuf stdout (Lexing.from_string s)

let main f =
  let inchan = open_in f in
  let outchan =
    let f = Filename.remove_extension f in
    match !backend_type with
    | Intel -> open_out (f ^ ".s")
    | ARM -> open_out (f ^ ".arm")
    | _ -> stdout
  in
  try
    let input = Lexing.from_channel inchan in
    with_flag ast_dump
      ~tru:(fun _ -> ast outchan input)
      ~fls:(fun _ -> lexbuf outchan input; close_in inchan; close_out outchan)
  with
  | e ->
    close_in inchan;
    close_out outchan;
    raise e

let () =
  let files = ref [] in
  Arg.parse
    [ ( "-inline", Arg.Int  (fun i -> Inline.threshold := i) , "maximum size of functions inlined" ) ;
      ( "-iter",   Arg.Int  (fun i -> Util.limit := i), "maximum number of optimizations iterated" ) ;
      ( "-ast",    Arg.Unit (fun _ -> ast_dump := true), "emit abstract syntax tree" ) ;
      ( "-intel" , Arg.Unit (fun _ -> backend_type := Intel) , "emit EM64T machine code" ) ;
      ( "-arm" ,   Arg.Unit (fun _ -> backend_type := ARM) , "emit AArch64 machine code" ) ;
      ( "-vm" ,    Arg.Unit (fun _ -> backend_type := Virtual) , "emit MinCaml IR virtual machine" ) ;
      ( "-debug",  Arg.Unit (fun _ -> debug := true), "enable debug mode" )
    ]
    (fun s -> files := !files @ [ s ])
    ("MinCaml EM64T/AArch64/IR Compiler (c) 2024 Namdak Tonpa\n"
    ^ "usage: joe [-inline m] [-iter n] ... filenames" );
  List.iter (fun f -> main f) !files

