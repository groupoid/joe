open MinCaml
module B = BacCaml

type backend =
  | Virtual
  | Bytecode
  | PPBytecode
  | Interp
  | Nothing

let backend_type = ref Bytecode
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

let rec lexbuf oc l =
  let open B in
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
  | Virtual -> p |> Emit.f |> Array.to_list |> Insts.Printer.pp_insts
  | PPBytecode -> p |> Emit.f |> Insts.Printer.pp_bytecode oc
  | Bytecode -> p |> Emit.f |> Insts.Printer.write_bytecode oc
  | Interp -> p |> Emit.f |> VM.run_asm |> ignore
  | Nothing -> ()
;;

let main f =
  let ic = open_in f in
  let oc = stdout in
  try
    let input = Lexing.from_channel ic in
    lexbuf oc input;
    close_in ic;
    close_out oc
  with
  | e ->
    close_in ic;
    close_out oc;
    raise e
;;

let () =
  let files = ref [] in
  B.(
    Arg.parse
      [ ( "-debug", Arg.Unit (fun _ -> debug_flg := true), "run as debug mode" ) ;
        ( "-inline", Arg.Int (fun i -> MinCaml.Inline.threshold := i), "set a threshold for inlining") ;
        ( "-iter", Arg.Int (fun i -> MinCaml.Util.limit := i), "set a threshold for iterating") ;
        ( "-no-sh", Arg.Unit (fun _ -> Config.(sh_flg := false)), "disable stack hybridization" ) ;
        ( "-virtual", Arg.Unit (fun _ -> backend_type := Virtual), "emit MinCaml IR" ) ;
        ( "-no-tail", Arg.Unit (fun _ -> Config.(tail_opt_flg := false)) , "enable optimization for tail-recursive call" ) ;
        ( "-no-fr", Arg.Unit (fun _ -> Config.frame_reset_flg := false), "disable to emit frame_reset" ) ;
        ( "-insts", Arg.Unit (fun _ -> show_insts_map_type := true), "show instruction map" ) ;
        ( "-pp", Arg.Unit (fun _ -> backend_type := PPBytecode), "emit bytecode for BacCaml" ) ;
        ( "-interp", Arg.Unit (fun _ -> backend_type := Interp), "run as interpreter" ) ;
        ( "-interp-hs", Arg.Unit (fun _ -> Config.stack_mode_flg := `Host_stack; backend_type := Interp), "running an interpreter using host-stack " )
      ])
    (fun s -> files := !files @ [ s ])
    (Sys.argv.(0) ^ " [-options] filename.ml");
  with_show_insts (fun _ -> with_debug (fun _ -> List.iter main !files))
;;
