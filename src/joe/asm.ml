type id_or_imm =
  | V of Id.t
  | C of int
[@@deriving show]

type t =
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
[@@deriving show]

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
[@@deriving show]

type fundef =
  { name : Id.l
  ; args : Id.t list
  ; fargs : Id.t list
  ; body : t
  ; ret : Type.t
  ; mutable annot : [ `TJ | `MJ ] option
  }
[@@deriving show]

(* プログラム全体 = 浮動小数点数テーブル + トップレベル関数 + メインの式 (caml2html: sparcasm_prog) *)
type prog = Prog of (Id.l * float) list * fundef list * t [@@deriving show]

let fletd (x, e1, e2) = Let ((x, Type.Float), e1, e2)
let seq (e1, e2) = Let ((Id.gentmp Type.Unit, Type.Unit), e1, e2)

module X86 = struct
  let regs = (* Array.init 16 (fun i -> Printf.sprintf "%%r%d" i) *) [| "%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi" |]
  let fregs = Array.init 8 (fun i -> Printf.sprintf "%%xmm%d" i)
  let allregs = Array.to_list regs
  let allfregs = Array.to_list fregs
  let reg_cl = regs.(Array.length regs - 1)
  (* closure address (caml2html: sparcasm_regcl) *)
  (* let reg_sw = regs.(Array.length regs - 1) (* temporary for swap *)
  let reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *) *)
  let reg_sp = "%ebp" (* stack pointer *)
  let reg_hp = "min_caml_hp" (* heap pointer (caml2html: sparcasm_reghp) *)
  (* let reg_ra = "%eax" (* return address *) *)
  let is_reg x = x.[0] = '%' || x = reg_hp
end

module X64 = struct
  let regs = (* Array.init 16 (fun i -> Printf.sprintf "%%r%d" i) *) [| "%rax"; "%rbx"; "%rcx"; "%rdx"; "%rsi"; "%rdi" |]
  let fregs = Array.init 8 (fun i -> Printf.sprintf "%%xmm%d" i)
  let allregs = Array.to_list regs
  let allfregs = Array.to_list fregs
  let reg_cl = regs.(Array.length regs - 1)
  (* closure address (caml2html: sparcasm_regcl) *)
  let reg_sp = "%rbp" (* stack pointer *)
  let reg_hp = "min_caml_hp" (* heap pointer (caml2html: sparcasm_reghp) *)
  let is_reg x = x.[0] = '%' || x = reg_hp
end

module ARM64 = struct
  let regs = (* Array.init 27 (fun i -> Printf.sprintf "_R_%d" i) *)
    [| "%x0"; "%x1"; "%x2"; "%x3"; "%x4"; "%x5"; "%x6"; "%x7"; "%x8"; "%x9"; "%x10";
    "%x11"; "%x12"; "%x13"; "%x14"; "%x15"; "%x16"; "%x17"; "%x18"; "%x19"; "%x20";
    "%x21"; "%x22"; "%x23"; "%x24"; "%x25" |]
  let fregs = Array.init 32 (fun i -> Printf.sprintf "%%d%d" i)
  let allregs = Array.to_list regs
  let allfregs = Array.to_list fregs
  let reg_cl = regs.(Array.length regs - 1) (* closure address (caml2html: sparcasm_regcl) *)
  let reg_sw = regs.(Array.length regs - 2) (* temporary for swap *)
  let reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *)
  let reg_sp = "%x28" (* stack pointer *)
  let reg_hp = "%x27" (* heap pointer (caml2html: sparcasm_reghp) *)
  let reg_tmp = "%x26" (* [XX] ad hoc *)
  let is_reg x = (x.[0] = '%')
end

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let fv_id_or_imm = function V x -> [ x ] | _ -> []

let rec fv_exp = function
  | Nop | SetL _ | Comment _ | Restore _ -> []
  | Mov x | Neg x | FMov x | FNeg x | Save (x, _) -> [ x ]
  | Add (x, y') | Mul (x, y') | Sub (x, y') | Div (x, y') | Mod (x, y')
  | Ld (x, y', _) | LdDF (x, y', _) -> x :: fv_id_or_imm y'
  | St (x, y, z', _) | StDF (x, y, z', _) -> x :: y :: fv_id_or_imm z'
  | Slw(x, y') | Lfd(x, y') | Lwz(x, y') -> x :: fv_id_or_imm y'
  | Stw(x, y, z') | Stfd(x, y, z') -> x :: y :: fv_id_or_imm z'
  | FAdd (x, y) | FSub (x, y) | FMul (x, y) | FDiv (x, y) -> [ x; y ]
  | IfEq (x, y', e1, e2) | IfLE (x, y', e1, e2) | IfGE (x, y', e1, e2) -> (x :: fv_id_or_imm y') @ remove_and_uniq S.empty (fv e1 @ fv e2)
  (* uniq here just for efficiency *)
  | IfFEq (x, y, e1, e2) | IfFLE (x, y, e1, e2) -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2)
  (* uniq here just for efficiency *)
  | CallCls (x, ys, zs) -> (x :: ys) @ zs
  | CallDir (_, ys, zs) -> ys @ zs
  | _ -> []

and fv = function
  | Ans exp -> fv_exp exp
  | Let ((x, t), exp, e) -> fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)

let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
  | Ans exp -> Let (xt, exp, e2)
  | Let (yt, exp, e1') -> Let (yt, exp, concat e1' xt e2)

let align i = if i mod 8 = 0 then i else i + 4

(* all local variables *)
let rec localvs_exp = function
  | IfEq (_, _, e1, e2)
  | IfLE (_, _, e1, e2)
  | IfGE (_, _, e1, e2)
  | IfFEq (_, _, e1, e2)
  | IfFLE (_, _, e1, e2) ->
    localvs e1 @ localvs e2
  | _ -> []

and localvs = function
  | Ans exp -> localvs_exp exp
  | Let (xt, exp, e) -> (xt :: localvs_exp exp) @ localvs e
