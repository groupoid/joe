open MinCaml
open Stdlib
open Printf

type backend = Compile | Interpret | Nothing

let backend_type = ref Compile
let show_insts_map_type = ref false
let debug_flg = ref false

let with_debug f =
    match !debug_flg with
    | true -> BacCaml.Config.vm_debug_flg := true; f ()
    | false -> f ()

let with_show_insts f =
    match !show_insts_map_type with
    | true -> BacCaml.Insts.Printer.pp_inst_map ()
    | false -> f ()

let rec normalForm l =
    let open BacCaml in
    let expression = Parser.exp Lexer.token l in
    expression |> Typing.f
               |> KNormal.f
               |> Alpha.f
               |> Util.(iter !limit)
               |> Closure.f
               |> Virtual.f
               |> Simm.f

let compileSource ml =
    let input = Lexing.from_channel ml in
    let res = normalForm input in
    let _ = close_in ml in res

let main f =
    let r x = match x with
              | BacCaml.Insts.Literal i -> Printf.printf "%d;" i
              | _ -> Printf.printf "%d;" (BacCaml.Insts.index_of x) in
    Id.counter := 0;
    Typing.extenv := M.empty;
    try match !backend_type with
      | Interpret ->
        let joe = open_in_bin ((Filename.remove_extension f) ^ ".joe") in
        let insts = (Marshal.from_channel joe) in BacCaml.VM.run_asm insts ; close_in joe
      | Compile ->
        let ml  = open_in ((Filename.remove_extension f) ^ ".ml") in
        let vm  = open_out_gen [Open_binary;Open_wronly;Open_creat]
                  0o644 ((Filename.remove_extension f) ^ ".joe") in
        let insts = (BacCaml.Emit.f (compileSource ml)) in
            Stdlib.output_bytes vm (Marshal.to_bytes insts [Marshal.No_sharing]) ; close_out vm
      | Nothing -> ()
    with | Invalid_argument _ -> ()
         | e -> raise e

let () =
  let files = ref [] in
  BacCaml.(
    Arg.parse
      [ ( "-debug", Arg.Unit (fun _ -> debug_flg := true), "run as debug mode" ) ;
        ( "-compile", Arg.Unit (fun _ ->  backend_type := Compile), "emit MinCaml IR" ) ;
        ( "-exec", Arg.Unit (fun _ -> backend_type := Interpret), "run IR in VM interpreter" ) ;
        ( "-no-tail", Arg.Unit (fun _ -> Config.tail_opt_flg := false), "disable optimization for tail-recursive call") ;
        ( "-no-fr", Arg.Unit (fun _ -> Config.frame_reset_flg := false), "disable to emit frame_reset" ) 
      ])
    (fun s -> files := !files @ [ s ])
    ( "MinCaml IR Virtual Machine (c) 2024 Namdak Tonpa\n"
    ^ "usage: vm [-options] filenames");
  with_show_insts (fun _ -> with_debug (fun _ -> List.iter main !files))

