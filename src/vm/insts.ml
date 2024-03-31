type inst =
  | UNIT
  | ADD
  | SUB
  | MUL
  | NOT
  | NEG
  | LT
  | EQ
  | JUMP_IF_ZERO
  | JUMP
  | CALL
  | RET
  | HALT
  | DUP
  | POP1
  | CONST
  | GET
  | PUT
  | ARRAY_MAKE
  | FRAME_RESET (* o l n *)
  | PRINT_INT
  | PRINT_NEWLINE
  | METHOD_ENTRY
  | CONST0
  | DUP0
  | METHOD_COMP
  | TRACING_COMP
  | JIT_SETUP
  | RAND_INT
  | DIV
  | MOD
  | GT
  | READ_INT
  | Literal of int
  | Lref of string
  | Ldef of string
[@@deriving show]

let insts =
  [| UNIT
   ; ADD
   ; SUB
   ; MUL
   ; NOT
   ; NEG
   ; LT
   ; EQ
   ; JUMP_IF_ZERO
   ; JUMP
   ; CALL
   ; RET
   ; HALT
   ; DUP
   ; POP1
   ; CONST
   ; GET
   ; PUT
   ; ARRAY_MAKE
   ; FRAME_RESET (* o l n *)
   ; PRINT_INT
   ; PRINT_NEWLINE
   ; METHOD_ENTRY
   ; CONST0
   ; DUP0
   ; METHOD_COMP                (* -1024 *)
   ; TRACING_COMP               (* -1048 *)
   ; JIT_SETUP
   ; RAND_INT
   ; DIV
   ; MOD
   ; GT
   ; READ_INT
  |]
;;

let index_of instr =
  match instr with
  | TRACING_COMP -> -1048
  | METHOD_COMP -> -1024
  | _ ->
    Array.to_list insts
    |> List.mapi (fun i instr -> instr, i)
    |> List.find (fun (instr', i) -> instr = instr')
    |> snd
;;

module Printer = struct
  let pp_inst_map () =
    ignore
      (Array.fold_left
         (fun i instr ->
           Printf.printf "%s => %d\n" (show_inst instr) i;
           i + 1)
         0
         insts)
  ;;

  let pp_insts_counter = ref 0

  let pp_pc () =
    print_int !pp_insts_counter;
    print_string "\t";
    incr pp_insts_counter
  ;;

  let rec pp_insts ?(i = 0) insts =
    match insts with
    | [] -> ()
    | hd :: tl ->
      (match hd with
      | CONST | DUP | JUMP | JUMP_IF_ZERO | RET ->
        pp_pc ();
        print_string (show_inst hd);
        print_string " ";
        pp_insts ~i:0 tl
      | CALL ->
        pp_pc ();
        print_string (show_inst hd);
        print_string " ";
        pp_insts ~i:1 tl
      | Literal n ->
        print_string "\t";
        print_string (show_inst hd);
        if i = 0 then print_newline () else print_string "\t";
        incr pp_insts_counter;
        pp_insts ~i:(i - 1) tl
      | _ ->
        pp_pc ();
        print_string (show_inst hd);
        print_newline ();
        pp_insts ~i:0 tl)
  ;;

  let rec pp_bytecode oc insts =
    insts
    |> Array.mapi (fun i x -> i, x)
    |> Array.map (fun (i, instr) ->
           match instr with
           | CONST | DUP | JUMP | JUMP_IF_ZERO | CALL | RET ->
             Printf.fprintf oc "code.(%d) <- %d; " i (index_of instr)
           | Literal j -> Printf.fprintf oc "code.(%d) <- %d;\n" i j
           | _ -> Printf.fprintf oc "code.(%d) <- %d;\n" i (index_of instr))
    |> ignore
  ;;

  let write_bytecode oc insts =
    Printf.fprintf oc "%d\n" (Array.length insts);
    insts
    |> Array.map (fun instr ->
           match instr with
           | Literal j -> Printf.fprintf oc "%d\n" j
           | _ -> Printf.fprintf oc "%d\n" (index_of instr))
    |> ignore
  ;;
end
