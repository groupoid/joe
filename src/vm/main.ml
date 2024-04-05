open MinCaml
open Format
open Printf
module B = BacCaml

type backend =
  | Compile
  | Interpret
  | Nothing

let backend_type = ref Compile
let show_insts_map_type = ref false
let debug_flg = ref false

let with_debug f =
  match !debug_flg with
  | true ->
    B.Config.vm_debug_flg := true;
    f ()
  | false -> f ()
;;

let with_show_insts f =
  match !show_insts_map_type with
  | true -> B.Insts.Printer.pp_inst_map ()
  | false -> f ()
;;

let rec lexbuf l =
  let open B in
  (Parser.exp Lexer.token l)
  |> Typing.f
  |> KNormal.f
  |> Alpha.f
  |> Util.(iter !limit)
  |> Closure.f
  |> Virtual.f
  |> Simm.f

let parseML ic =
    let input = Lexing.from_channel ic in
    let res = lexbuf input in
    let _ = close_in ic in res

let main f =
    let open B in
    Id.counter := 0;
    Typing.extenv := M.empty;
    try
      let r x = match x with
              | Insts.Literal i -> Printf.printf "%d;" i
              | _ -> Printf.printf "%d;" (Insts.index_of x) in
      match !backend_type with
      | Interpret ->
        let joe = open_in_bin ((Filename.remove_extension f) ^ ".joe") in
        let insts = (Marshal.from_channel joe) in
        let _ = Array.map r insts in VM.run_asm insts ; close_in joe ; ()
      | Compile ->
        let ml  = open_in ((Filename.remove_extension f) ^ ".ml") in
        let vm  = open_out_bin ((Filename.remove_extension f) ^ ".joe") in
        let insts = (Emit.f (parseML ml)) in
        let _ = Array.map r insts in Stdlib.output_bytes vm (Marshal.to_bytes insts [Marshal.No_sharing])
        ; close_in ml ; close_out vm
      | Nothing -> ()
    with | e -> raise e

let () =
  let files = ref [] in
  B.(
    Arg.parse
      [ ( "-debug", Arg.Unit (fun _ -> debug_flg := true), "run as debug mode" ) ;
        ( "-inline", Arg.Int (fun i -> MinCaml.Inline.threshold := i), "set a threshold for inlining") ;
        ( "-iter", Arg.Int (fun i -> MinCaml.Util.limit := i), "set a threshold for iterating") ;
        ( "-no-sh", Arg.Unit (fun _ -> Config.(sh_flg := false)), "disable stack hybridization" ) ;
        ( "-compile", Arg.Unit (fun _ -> backend_type := Compile), "emit MinCaml IR" ) ;
        ( "-exec", Arg.Unit (fun _ -> backend_type := Interpret), "run IR in VM interpreter" ) ;
        ( "-no-tail", Arg.Unit (fun _ -> Config.(tail_opt_flg := false)) , "enable optimization for tail-recursive call" ) ;
        ( "-no-fr", Arg.Unit (fun _ -> Config.frame_reset_flg := false), "disable to emit frame_reset" ) 
      ])
    (fun s -> files := !files @ [ s ])
    ( "MinCaml IR Virtual Machine (c) 2024 Namdak Tonpa\n"
    ^ "usage: vm [-options] filenames");
  with_show_insts (fun _ -> with_debug (fun _ -> List.iter main !files))
;;
