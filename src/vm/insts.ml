type inst =
   | UNIT                                            (* terminator *)
   | ADD | SUB | MUL | DIV | MOD | NOT | NEG (* binary and ALU ops *)
   | LT | GT | EQ                                      (* equality *)
   | JUMP_IF_ZERO | JUMP | CALL | RET | HALT       (* control flow *)
   | DUP | DUP0 | POP1                                (* stack ops *)
   | CONST0 | CONST                                   (* constants *)
   | GET | PUT | ARRAY_MAKE                             (* vectors *)
   | FRAME_RESET | JIT_SETUP                              (* o l n *)
   | RAND_INT | READ_INT | READ_STRING                  (* io read *)
   | PRINT_INT | PRINT_NEWLINE | PRINT_STRING          (* io write *)
   | Literal of int
   | Lref of string
   | Ldef of string [@@deriving show]

let instsmap =
  [| UNIT
   ; ADD ; SUB ; MUL ; DIV ; MOD ; NOT ; NEG
   ; LT ; GT ; EQ
   ; JUMP_IF_ZERO ; JUMP ; CALL ; RET ; HALT
   ; DUP ; DUP0 ; POP1
   ; CONST0 ; CONST
   ; GET ; PUT ; ARRAY_MAKE
   ; FRAME_RESET (* o l n *) ; JIT_SETUP
   ; RAND_INT ; READ_INT ; READ_STRING
   ; PRINT_INT ; PRINT_NEWLINE ; PRINT_STRING
  |]

let index_of instr = match instr with
   | UNIT -> 0 | ADD -> 1 | SUB -> 2 | MUL -> 3
   | DIV -> 4 | MOD -> 5 | NOT -> 6 | NEG -> 7
   | LT -> 8 | GT -> 9 | EQ -> 10 | HALT -> 11
   | JUMP_IF_ZERO -> 12 | JUMP -> 13 | CALL -> 14 | RET -> 15
   | DUP -> 16 | DUP0 -> 17 | POP1 -> 18 | CONST0 -> 19
   | CONST -> 20 | GET -> 21 | PUT -> 22 | ARRAY_MAKE -> 23
   | FRAME_RESET -> 24 | JIT_SETUP -> 25
   | RAND_INT -> 26 | READ_INT -> 27 | READ_STRING -> 28
   | PRINT_INT -> 29 | PRINT_NEWLINE -> 30 | PRINT_STRING -> 31
   | _ -> 0

module Printer = struct
  let pp_inst_map () =
    ignore
      (Array.fold_left
         (fun i instr ->
           Printf.printf "%s => %d\n" (show_inst instr) i;
           i + 1)
         0
         instsmap)
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
