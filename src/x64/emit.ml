open MinCaml
open Asm
open Asm.X64

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

let stackset = ref S.empty  (* ã™ã§ã«Saveã•ã‚ŒãŸå¤‰æ•°ã®é›†åˆ (caml2html: emit_stackset) *)
let stackmap = ref [] (* Saveã•ã‚ŒãŸå¤‰æ•°ã®ã€ã‚¹ã‚¿ãƒƒã‚¯ã«ãŠã‘ã‚‹ä½ç½® (caml2html: emit_stackmap) *)

let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then stackmap := !stackmap @ [ x ]

let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap)
  then (
    let pad =
      if List.length !stackmap mod 2 = 0 then [] else [ Id.gentmp Type.Int ]
    in
    stackmap := !stackmap @ pad @ [ x; x ])

let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs)
  in
  loc !stackmap
;;

(* NOTE: 64ãƒ“ãƒƒãƒˆãªã®ã§ 4 ãƒã‚¤ãƒˆã‹ã‚‰ 8 ãƒã‚¤ãƒˆã«ä¿®æ­£ *)
let offset x = 8 * List.hd (locate x) (*4->8*)
let stacksize () = align (List.length !stackmap * 8) (*4->8*)

let pp_id_or_imm = function V x -> x | C i -> "$" ^ string_of_int i

(* é–¢æ•°å‘¼ã³å‡ºã—ã®ãŸã‚ã«å¼•æ•°ã‚’ä¸¦ã¹æ›¿ãˆã‚‹(register shuffling) (caml2html: emit_shuffle) *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] ->
    (* no acyclic moves; resolve a cyclic move *)
    (y, sw)
    :: (x, y)
    :: shuffle
         sw
         (List.map (function y', z when y = y' -> sw, z | yz -> yz) xys)
  | xys, acyc -> acyc @ shuffle sw xys
;;

type dest = Tail | NonTail of Id.t (* æœ«å°¾ã‹ã©ã†ã‹ã‚’è¡¨ã™ãƒ‡ãƒ¼ã‚¿åž‹ (caml2html: emit_dest) *)

let rec g oc = function (* å‘½ä»¤åˆ—ã®ã‚¢ã‚»ãƒ³ãƒ–ãƒªç”Ÿæˆ (caml2html: emit_g) *)
  | dest, Ans exp -> g' oc (dest, exp)
  | dest, Let ((x, t), exp, e) ->
    g' oc (NonTail x, exp);
    g oc (dest, e)

and g' oc = function (* å„å‘½ä»¤ã®ã‚¢ã‚»ãƒ³ãƒ–ãƒªç”Ÿæˆ (caml2html: emit_gprime) *)
  (* æœ«å°¾ã§ãªã‹ã£ãŸã‚‰è¨ˆç®—çµæžœã‚’destã«ã‚»ãƒƒãƒˆ (caml2html: emit_nontail) *)
  | NonTail _, Nop -> ()
  | NonTail x, Set i -> Printf.fprintf oc "\tmovq\t$%d, %s\n" i x
  | NonTail x, SetL (Id.L y) -> Printf.fprintf oc "\tmovq\t$%s, %s\n" y x
  | NonTail x, Mov y -> if x <> y then Printf.fprintf oc "\tmovq\t%s, %s\n" y x
  | NonTail x, Neg y ->
    if x <> y then Printf.fprintf oc "\tmovq\t%s, %s\n" y x;
    Printf.fprintf oc "\tnegq\t%s\n" x
  | NonTail x, Add (y, z') ->
    if V x = z'
    then Printf.fprintf oc "\taddq\t%s, %s\n" y x
    else (
      if x <> y then Printf.fprintf oc "\tmovq\t%s, %s\n" y x;
      Printf.fprintf oc "\taddq\t%s, %s\n" (pp_id_or_imm z') x)
  | NonTail x, Sub (y, z') ->
    if V x = z'
    then (
      Printf.fprintf oc "\tsubq\t%s, %s\n" y x;
      Printf.fprintf oc "\tnegq\t%s\n" x)
    else (
      if x <> y then Printf.fprintf oc "\tmovq\t%s, %s\n" y x;
      Printf.fprintf oc "\tsubq\t%s, %s\n" (pp_id_or_imm z') x)
  | NonTail x, Mul (y, z') ->
    if V x = z'
    then Printf.fprintf oc "\tmul\t%s, %s\n" y x
    else (
      if x <> y then Printf.fprintf oc "\tmovq\t%s, %s\n" y x;
      Printf.fprintf oc "\tmul\t%s, %s\n" (pp_id_or_imm z') x)
  | NonTail x, Ld (y, V z, i) ->
    Printf.fprintf oc "\tmovq\t(%s,%s,%d), %s\n" y z i x
  | NonTail x, Ld (y, C j, i) ->
    Printf.fprintf oc "\tmovq\t%d(%s), %s\n" (j * i) y x
  | NonTail _, St (x, y, V z, i) ->
    Printf.fprintf oc "\tmovq\t%s, (%s,%s,%d)\n" x y z i
  | NonTail _, St (x, y, C j, i) ->
    Printf.fprintf oc "\tmovq\t%s, %d(%s)\n" x (j * i) y
  | NonTail x, FMov y ->
    if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x
  | NonTail x, FNeg y ->
    if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
    Printf.fprintf oc "\txorpd\tmin_caml_fnegd, %s\n" x
  | NonTail x, FAdd (y, z) ->
    if x = z
    then Printf.fprintf oc "\taddsd\t%s, %s\n" y x
    else (
      if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
      Printf.fprintf oc "\taddsd\t%s, %s\n" z x)
  | NonTail x, FSub (y, z) ->
    if x = z
    then (
      (* [XXX] ugly *)
      let ss = stacksize () in
      Printf.fprintf oc "\tmovsd\t%s, %d(%s)\n" z ss reg_sp;
      if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
      Printf.fprintf oc "\tsubsd\t%d(%s), %s\n" ss reg_sp x)
    else (
      if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
      Printf.fprintf oc "\tsubsd\t%s, %s\n" z x)
  | NonTail x, FMul (y, z) ->
    if x = z
    then Printf.fprintf oc "\tmulsd\t%s, %s\n" y x
    else (
      if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
      Printf.fprintf oc "\tmulsd\t%s, %s\n" z x)
  | NonTail x, FDiv (y, z) ->
    if x = z
    then (
      (* [XXX] ugly *)
      let ss = stacksize () in
      Printf.fprintf oc "\tmovsd\t%s, %d(%s)\n" z ss reg_sp;
      if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
      Printf.fprintf oc "\tdivsd\t%d(%s), %s\n" ss reg_sp x)
    else (
      if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
      Printf.fprintf oc "\tdivsd\t%s, %s\n" z x)
  | NonTail x, LdDF (y, V z, i) ->
    Printf.fprintf oc "\tmovsd\t(%s,%s,%d), %s\n" y z i x
  | NonTail x, LdDF (y, C j, i) ->
    Printf.fprintf oc "\tmovsd\t%d(%s), %s\n" (j * i) y x
  | NonTail _, StDF (x, y, V z, i) ->
    Printf.fprintf oc "\tmovsd\t%s, (%s,%s,%d)\n" x y z i
  | NonTail _, StDF (x, y, C j, i) ->
    Printf.fprintf oc "\tmovsd\t%s, %d(%s)\n" x (j * i) y
  | NonTail _, Comment s -> Printf.fprintf oc "\t# %s\n" s
  (* ÂàÈò¤Î²¾ÁÛÌ¿Îá¤Î¼ÂÁõ (caml2html: emit_save) *)
  | NonTail _, Save (x, y) when List.mem x allregs && not (S.mem y !stackset) ->
    save y;
    Printf.fprintf oc "\tmovq\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail _, Save (x, y) when List.mem x allfregs && not (S.mem y !stackset)
    ->
    savef y;
    Printf.fprintf oc "\tmovsd\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail _, Save (x, y) ->
  (*  assert (S.mem y !stackset); *)
    ()
  (* Éüµ¢¤Î²¾ÁÛÌ¿Îá¤Î¼ÂÁõ (caml2html: emit_restore) *)
  | NonTail x, Restore y when List.mem x allregs ->
    Printf.fprintf oc "\tmovq\t%d(%s), %s\n" (offset y) reg_sp x
  | NonTail x, Restore y ->
(*    assert (List.mem x allfregs); *)
    Printf.fprintf oc "\tmovsd\t%d(%s), %s\n" (offset y) reg_sp x
  (* ËöÈø¤À¤Ã¤¿¤é·×»»·ë²Ì¤òÂè°ì¥ì¥¸¥¹¥¿¤Ë¥»¥Ã¥È¤·¤Æret (caml2html: emit_tailret) *)
  | Tail, ((Nop | St _ | StDF _ | Comment _ | Save _) as exp) ->
    g' oc (NonTail (Id.gentmp Type.Unit), exp);
    Printf.fprintf oc "\tret\n"
  | ( Tail
    , ((Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Mul _ | Ld _) as exp) )
    ->
    g' oc (NonTail regs.(0), exp);
    Printf.fprintf oc "\tret\n"
  | ( Tail
    , ((FMov _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ | LdDF _) as
      exp) ) ->
    g' oc (NonTail fregs.(0), exp);
    Printf.fprintf oc "\tret\n"
  | Tail, (Restore x as exp) ->
    (match locate x with
    | [ i ] -> g' oc (NonTail regs.(0), exp)
    | [ i; j ] when i + 1 = j -> g' oc (NonTail fregs.(0), exp)
    | _ -> assert false);
    Printf.fprintf oc "\tret\n"
  | Tail, IfEq (x, y', e1, e2) ->
    Printf.fprintf oc "\tcmpq\t%s, %s\n" (pp_id_or_imm y') x;
    g'_tail_if oc e1 e2 "je" "jne"
  | Tail, IfLE (x, y', e1, e2) ->
    Printf.fprintf oc "\tcmpq\t%s, %s\n" (pp_id_or_imm y') x;
    g'_tail_if oc e1 e2 "jle" "jg"
  | Tail, IfGE (x, y', e1, e2) ->
    Printf.fprintf oc "\tcmpq\t%s, %s\n" (pp_id_or_imm y') x;
    g'_tail_if oc e1 e2 "jge" "jl"
  | Tail, IfFEq (x, y, e1, e2) ->
    Printf.fprintf oc "\tcomisd\t%s, %s\n" y x;
    g'_tail_if oc e1 e2 "je" "jne"
  | Tail, IfFLE (x, y, e1, e2) ->
    Printf.fprintf oc "\tcomisd\t%s, %s\n" y x;
    g'_tail_if oc e1 e2 "jbe" "ja"
  | NonTail z, IfEq (x, y', e1, e2) ->
    Printf.fprintf oc "\tcmpq\t%s, %s\n" (pp_id_or_imm y') x;
    g'_non_tail_if oc (NonTail z) e1 e2 "je" "jne"
  | NonTail z, IfLE (x, y', e1, e2) ->
    Printf.fprintf oc "\tcmpq\t%s, %s\n" (pp_id_or_imm y') x;
    g'_non_tail_if oc (NonTail z) e1 e2 "jle" "jg"
  | NonTail z, IfGE (x, y', e1, e2) ->
    Printf.fprintf oc "\tcmpq\t%s, %s\n" (pp_id_or_imm y') x;
    g'_non_tail_if oc (NonTail z) e1 e2 "jge" "jl"
  | NonTail z, IfFEq (x, y, e1, e2) ->
    Printf.fprintf oc "\tcomisd\t%s, %s\n" y x;
    g'_non_tail_if oc (NonTail z) e1 e2 "je" "jne"
  | NonTail z, IfFLE (x, y, e1, e2) ->
    Printf.fprintf oc "\tcomisd\t%s, %s\n" y x;
    g'_non_tail_if oc (NonTail z) e1 e2 "jbe" "ja"
  (* ´Ø¿ô¸Æ¤Ó½Ð¤·¤Î²¾ÁÛÌ¿Îá¤Î¼ÂÁõ (caml2html: emit_call) *)
  | Tail, CallCls (x, ys, zs) ->
    (* ËöÈø¸Æ¤Ó½Ð¤· (caml2html: emit_tailcall) *)
    g'_args oc [ x, reg_cl ] ys zs;
    Printf.fprintf oc "\tjmp\t*(%s)\n" reg_cl
  | Tail, CallDir (Id.L x, ys, zs) ->
    (* ËöÈø¸Æ¤Ó½Ð¤· *)
    g'_args oc [] ys zs;
    Printf.fprintf oc "\tjmp\t%s\n" x
  | NonTail a, CallCls (x, ys, zs) ->
    g'_args oc [ x, reg_cl ] ys zs;
    let ss = stacksize () in
    if ss > 0 then Printf.fprintf oc "\taddq\t$%d, %s\n" ss reg_sp;
    Printf.fprintf oc "\tcall\t*(%s)\n" reg_cl;
    if ss > 0 then Printf.fprintf oc "\tsubq\t$%d, %s\n" ss reg_sp;
    if List.mem a allregs && a <> regs.(0)
    then Printf.fprintf oc "\tmovq\t%s, %s\n" regs.(0) a
    else if List.mem a allfregs && a <> fregs.(0)
    then Printf.fprintf oc "\tmovsd\t%s, %s\n" fregs.(0) a
  | NonTail a, CallDir (Id.L x, ys, zs) ->
    g'_args oc [] ys zs;
    let ss = stacksize () in
    if ss > 0 then Printf.fprintf oc "\taddq\t$%d, %s\n" ss reg_sp;
    Printf.fprintf oc "\tcall\t%s\n" x;
    if ss > 0 then Printf.fprintf oc "\tsubq\t$%d, %s\n" ss reg_sp;
    if List.mem a allregs && a <> regs.(0)
    then Printf.fprintf oc "\tmovq\t%s, %s\n" regs.(0) a
    else if List.mem a allfregs && a <> fregs.(0)
    then Printf.fprintf oc "\tmovsd\t%s, %s\n" fregs.(0) a

and g'_tail_if oc e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)

and g'_non_tail_if oc dest e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tjmp\t%s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2

and g'_args oc x_reg_cl ys zs =
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let sw = Printf.sprintf "%d(%s)" (stacksize ()) reg_sp in
  let i, yrs =
    List.fold_left
      (fun (i, yrs) y -> i + 1, (y, regs.(i)) :: yrs)
      (0, x_reg_cl)
      ys
  in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "\tmovq\t%s, %s\n" y r)
    (shuffle sw yrs);
  let d, zfrs =
    List.fold_left (fun (d, zfrs) z -> d + 1, (z, fregs.(d)) :: zfrs) (0, []) zs
  in
  List.iter
    (fun (z, fr) -> Printf.fprintf oc "\tmovsd\t%s, %s\n" z fr)
    (shuffle sw zfrs)
;;

let h oc { name = Id.L x; args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)
;;

let f oc (Prog (data, fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc ".data\n";
  Printf.fprintf oc ".balign\t8\n";
  List.iter
    (fun (Id.L x, d) ->
      Printf.fprintf oc "%s:\t# %f\n" x d;
      Printf.fprintf oc "\t.long\t0x%lx\n" (gethi d);
      Printf.fprintf oc "\t.long\t0x%lx\n" (getlo d))
    data;
  Printf.fprintf oc ".text\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc ".globl\tmin_caml_start\n";
  Printf.fprintf oc "min_caml_start:\n";
  Printf.fprintf oc ".globl\t_min_caml_start\n";
  Printf.fprintf oc "_min_caml_start: # for cygwin\n";
  Printf.fprintf oc "\tpushq\t%%rax\n";
  Printf.fprintf oc "\tpushq\t%%rbx\n";
  Printf.fprintf oc "\tpushq\t%%rcx\n";
  Printf.fprintf oc "\tpushq\t%%rdx\n";
  Printf.fprintf oc "\tpushq\t%%rsi\n";
  Printf.fprintf oc "\tpushq\t%%rdi\n";
  Printf.fprintf oc "\tpushq\t%%rbp\n";
  Printf.fprintf oc "\tmovq\t%%rdi,%s\n" reg_sp;
  Printf.fprintf oc "\tmovq\t%%rsi,%s\n" regs.(0);
  Printf.fprintf oc "\tmovq\t%s,%s\n" regs.(0) reg_hp;
  stackset := S.empty;
  stackmap := [];
  g oc (NonTail regs.(0), e);
  Printf.fprintf oc "\tpopq\t%%rbp\n";
  Printf.fprintf oc "\tpopq\t%%rdi\n";
  Printf.fprintf oc "\tpopq\t%%rsi\n";
  Printf.fprintf oc "\tpopq\t%%rdx\n";
  Printf.fprintf oc "\tpopq\t%%rcx\n";
  Printf.fprintf oc "\tpopq\t%%rbx\n";
  Printf.fprintf oc "\tpopq\t%%rax\n";
  Printf.fprintf oc "\tret\n"
;;
