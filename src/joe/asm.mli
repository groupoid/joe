type id_or_imm =
  | V of Id.t
  | C of int

type t =
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t

and exp =
  | Nop
  | Li    of int
  | FLi   of Id.l
  | SetL  of Id.l
  | Mr    of Id.t
  | Mov   of Id.t
  (* ALU instructions *)
  | Neg   of Id.t
  | Add   of Id.t * id_or_imm
  | Sub   of Id.t * id_or_imm
  | Mul   of Id.t * id_or_imm
  | Div   of Id.t * id_or_imm
  | Mod   of Id.t * id_or_imm
  (* load/store instructions *)
  | Lfd   of Id.t * id_or_imm
  | Lwz   of Id.t * id_or_imm
  | Ld    of Id.t * id_or_imm * int
  | Slw   of Id.t * id_or_imm
  | Stw   of Id.t * Id.t * id_or_imm
  | Stfd  of Id.t * Id.t * id_or_imm
  | St    of Id.t * Id.t * id_or_imm * int
  (* float instructions *)
  | FMov  of Id.t
  | FNeg  of Id.t
  | FAdd  of Id.t * Id.t
  | FSub  of Id.t * Id.t
  | FMul  of Id.t * Id.t
  | FDiv  of Id.t * Id.t
  | LdDF  of Id.t * id_or_imm * int
  | StDF  of Id.t * Id.t * id_or_imm * int
  | FMr   of Id.t
  | Comment  of string
  (* virtual instructions *)
  | IfEq of Id.t * id_or_imm * t * t
  | IfLE of Id.t * id_or_imm * t * t
  | IfGE of Id.t * id_or_imm * t * t (* 左右対称ではないので必要 *)
  | IfFEq of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 (caml2html: sparcasm_save) *)
  | Restore of Id.t (* スタック変数から値を復元 (caml2html: sparcasm_restore) *)

type fundef =
  { name : Id.l
  ; args : Id.t list
  ; fargs : Id.t list
  ; body : t
  ; ret : Type.t
  ; mutable annot : [ `TJ | `MJ ] option
  }

type prog = Prog of (Id.l * float) list * fundef list * t

val show : t -> string
val show_exp : exp -> string
val show_fundef : fundef -> string
val show_prog : prog -> string
val fletd : Id.t * exp * t -> t (* shorthand of Let for float *)

val seq : exp * t -> t (* shorthand of Let for unit *)

module X86 : sig
  val regs : Id.t array
  val fregs : Id.t array
  val allregs : Id.t list
  val allfregs : Id.t list
  val reg_cl : Id.t
  (* val reg_sw : Id.t
     val reg_fsw : Id.t
     val reg_ra : Id.t *)
  val reg_hp : Id.t
  val reg_sp : Id.t
  val is_reg : Id.t -> bool
end

module X64 : sig
  val regs : Id.t array
  val fregs : Id.t array
  val allregs : Id.t list
  val allfregs : Id.t list
  val reg_cl : Id.t
  val reg_hp : Id.t
  val reg_sp : Id.t
  val is_reg : Id.t -> bool
end

module ARM64 : sig
  val regs : Id.t array
  val fregs  : Id.t array
  val allregs : Id.t list
  val allfregs : Id.t list
  val reg_cl : Id.t
  val reg_sw : Id.t
  val reg_fsw : Id.t
  val reg_sp : Id.t
  val reg_hp : Id.t
  val reg_tmp : Id.t
  val is_reg : Id.t -> bool
end

val fv : t -> Id.t list
val concat : t -> Id.t * Type.t -> t -> t
val align : int -> int
val localvs : t -> (Id.t * Type.t) list
