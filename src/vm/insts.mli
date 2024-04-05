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
   | Ldef of string

val index_of : inst -> int
val show_inst : inst -> string
val instsmap : inst array

module Printer : sig
  val pp_inst_map : unit -> unit
  val pp_insts : ?i:int -> inst list -> unit
  val pp_bytecode : out_channel -> inst array -> unit
  val write_bytecode : out_channel -> inst array -> unit
end
