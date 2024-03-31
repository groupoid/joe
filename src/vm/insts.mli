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

val show_inst : inst -> string
val insts : inst array

module Printer : sig
  val pp_inst_map : unit -> unit
  val pp_insts : ?i:int -> inst list -> unit
  val pp_bytecode : out_channel -> inst array -> unit
  val write_bytecode : out_channel -> inst array -> unit
end
