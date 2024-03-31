open MinCaml
open Asm
open Asm.ARM64

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

let stackset = ref S.empty (* すでにSaveされた変数の集合 (caml2html: emit_stackset) *)
let stackmap = ref [] (* Saveされた変数の、スタックにおける位置 (caml2html: emit_stackmap) *)

let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad =
      if List.length !stackmap mod 2 = 0 then [] else [Id.gentmp Type.Int] in
    stackmap := !stackmap @ pad @ [x; x])
let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
  loc !stackmap

(* NOTE: 64ビットなので 4 バイトから 8 バイトに修正 *)
let offset x = 8 * List.hd (locate x)
let stacksize () = align ((List.length !stackmap + 1) * 8)

let reg r =
  if is_reg r
  then String.sub r 1 (String.length r - 1)
  else r

(* 関数呼び出しのために引数を並べ替える(register shuffling) (caml2html: emit_shuffle) *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] -> (* no acyclic moves; resolve a cyclic move *)
      (y, sw) :: (x, y) :: shuffle sw (List.map
                                         (function
                                           | (y', z) when y = y' -> (sw, z)
                                           | yz -> yz)
                                         xys)
  | xys, acyc -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t (* 末尾かどうかを表すデータ型 (caml2html: emit_dest) *)
let rec g oc = function (* 命令列のアセンブリ生成 (caml2html: emit_g) *)
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
      g' oc (NonTail(x), exp);
      g oc (dest, e)
and g' oc = function (* 各命令のアセンブリ生成 (caml2html: emit_gprime) *)
  (* 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail) *)
  | NonTail(_), Nop -> ()
  | NonTail(x), Li(i) when -32768 <= i && i < 32768 -> Printf.fprintf oc "\tmov %s, %d\n" (reg x) i
  | NonTail(x), Li(i) ->
      (* 16ビットずつに区切って定数を読み込む *)
      let a = i land 0xffff in
      let b = (i lsr 16) land 0xffff in
      let c = (i lsr 32) land 0xffff in
      let d = (i lsr 48) land 0xffff in
      Printf.fprintf oc "\tmov %s, %d\n" (reg x) a;
      Printf.fprintf oc "\tmovk %s, %d, lsl 16\n" (reg x) b;
      Printf.fprintf oc "\tmovk %s, %d, lsl 32\n" (reg x) c;
      Printf.fprintf oc "\tmovk %s, %d, lsl 48\n" (reg x) d
  | NonTail(x), FLi(Id.L(l)) ->
      (* ラベル l に格納された浮動小数点数をレジスタへロードする *)
      Printf.fprintf oc "\tadrp %s, %s@PAGE\n" (reg reg_tmp) l;
      Printf.fprintf oc "\tldr %s, [%s, %s@PAGEOFF]\n" (reg x) (reg reg_tmp) l
  | NonTail(x), SetL(Id.L(y)) ->
      Printf.fprintf oc "\tadrp %s, %s@PAGE\n" (reg x) y;
      Printf.fprintf oc "\tadd %s, %s, %s@PAGEOFF\n" (reg x) (reg x) y
  | NonTail(x), Mr(y) when x = y -> ()
  | NonTail(x), Mr(y) -> Printf.fprintf oc "\tmov %s, %s\n" (reg x) (reg y)
  | NonTail(x), Neg(y) -> Printf.fprintf oc "\tneg\t%s, %s\n" (reg x) (reg y)
  | NonTail(x), Add(y, V(z)) -> Printf.fprintf oc "\tadd\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | NonTail(x), Add(y, C(z)) -> Printf.fprintf oc "\tadd %s, %s, %d\n" (reg x) (reg y) z
  | NonTail(x), Sub(y, V(z)) -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | NonTail(x), Sub(y, C(z)) -> Printf.fprintf oc "\tsub %s, %s, %d\n" (reg x) (reg y) z
  | NonTail(x), Slw(y, V(z)) -> Printf.fprintf oc "\tslw\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | NonTail(x), Slw(y, C(z)) -> Printf.fprintf oc "\tlsl %s, %s, %d\n" (reg x) (reg y) z
  | NonTail(x), Lwz(y, V(z)) -> Printf.fprintf oc "\tldr %s, [%s, %s]\n" (reg x) (reg y) (reg z)
  | NonTail(x), Lwz(y, C(z)) -> Printf.fprintf oc "\tldr %s, [%s, %d]\n" (reg x) (reg y) z
  | NonTail(_), Stw(x, y, V(z)) -> Printf.fprintf oc "\tstr %s, [%s, %s]\n" (reg x) (reg y) (reg z)
  | NonTail(_), Stw(x, y, C(z)) -> Printf.fprintf oc "\tstr %s, [%s, %d]\n" (reg x) (reg y) z
  | NonTail(x), FMr(y) when x = y -> ()
  | NonTail(x), FMr(y) -> Printf.fprintf oc "\tfmr\t%s, %s\n" (reg x) (reg y)
  | NonTail(x), FNeg(y) -> Printf.fprintf oc "\tfneg\t%s, %s\n" (reg x) (reg y)
  | NonTail(x), FAdd(y, z) -> Printf.fprintf oc "\tfadd\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | NonTail(x), FSub(y, z) -> Printf.fprintf oc "\tfsub\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | NonTail(x), FMul(y, z) -> Printf.fprintf oc "\tfmul\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | NonTail(x), FDiv(y, z) -> Printf.fprintf oc "\tfdiv\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | NonTail(x), Lfd(y, V(z)) -> Printf.fprintf oc "\tldr %s, [%s, %s]\n" (reg x) (reg y) (reg z)
  | NonTail(x), Lfd(y, C(z)) -> Printf.fprintf oc "\tldr %s, [%s, %d]\n" (reg x) (reg y) z
  | NonTail(_), Stfd(x, y, V(z)) -> Printf.fprintf oc "\tstr %s, [%s, %s]\n" (reg x) (reg y) (reg z)
  | NonTail(_), Stfd(x, y, C(z)) -> Printf.fprintf oc "\tstr %s, [%s, %d]\n" (reg x) (reg y) z
  | NonTail(_), Comment(s) -> Printf.fprintf oc "#\t%s\n" s
  (* 退避の仮想命令の実装 (caml2html: emit_save) *)
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      Printf.fprintf oc "\tstr %s, [%s, %d]\n" (reg x) (reg reg_sp) (offset y)
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
      savef y;
      Printf.fprintf oc "\tstr %s, [%s, %d]\n" (reg x) (reg reg_sp) (offset y)
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  (* 復帰の仮想命令の実装 (caml2html: emit_restore) *)
  | NonTail(x), Restore(y) when List.mem x allregs ->
      Printf.fprintf oc "\tldr %s, [%s, %d]\n" (reg x) (reg reg_sp) (offset y)
  | NonTail(x), Restore(y) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tldr %s, [%s, %d]\n" (reg x) (reg reg_sp) (offset y)
  (* 末尾だったら計算結果を第一レジスタにセットしてリターン (caml2html: emit_tailret) *)
  | Tail, (Nop | Stw _ | Stfd _ | Comment _ | Save _ as exp) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tret\n";
  | Tail, (Li _ | SetL _ | Mr _ | Neg _ | Add _ | Sub _ | Slw _ | Lwz _ as exp) ->
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "\tret\n";
  | Tail, (FLi _ | FMr _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ | Lfd _ as exp) ->
      g' oc (NonTail(fregs.(0)), exp);
      Printf.fprintf oc "\tret\n";
  | Tail, (Restore(x) as exp) ->
      (match locate x with
      | [i] -> g' oc (NonTail(regs.(0)), exp)
      | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp)
      | _ -> assert false);
      Printf.fprintf oc "\tret\n";
  | Tail, IfEq(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %s\n" (reg x) (reg y);
      g'_tail_if oc e1 e2 "beq" "bne"
  | Tail, IfEq(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %d\n" (reg x) y;
      g'_tail_if oc e1 e2 "beq" "bne"
  | Tail, IfLE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %s\n" (reg x) (reg y);
      g'_tail_if oc e1 e2 "ble" "bgt"
  | Tail, IfLE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %d\n" (reg x) y;
      g'_tail_if oc e1 e2 "ble" "bgt"
  | Tail, IfGE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %s\n" (reg x) (reg y);
      g'_tail_if oc e1 e2 "bge" "blt"
  | Tail, IfGE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %d\n" (reg x) y;
      g'_tail_if oc e1 e2 "bge" "blt"
  | Tail, IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmp %s, %s\n" (reg x) (reg y);
      g'_tail_if oc e1 e2 "beq" "bne"
  | Tail, IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmp %s, %s\n" (reg x) (reg y);
      g'_tail_if oc e1 e2 "ble" "bgt"
  | NonTail(z), IfEq(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %s\n" (reg x) (reg y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" "bne"
  | NonTail(z), IfEq(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %d\n" (reg x) y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" "bne"
  | NonTail(z), IfLE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %s\n" (reg x) (reg y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bgt"
  | NonTail(z), IfLE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %d\n" (reg x) y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bgt"
  | NonTail(z), IfGE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %s\n" (reg x) (reg y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bge" "blt"
  | NonTail(z), IfGE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmp %s, %d\n" (reg x) y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bge" "blt"
  | NonTail(z), IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmp %s, %s\n" (reg x) (reg y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" "bne"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmp %s, %s\n" (reg x) (reg y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bgt"
  (* 関数呼び出しの仮想命令の実装 (caml2html: emit_call) *)
  | Tail, CallCls(x, ys, zs) -> (* 末尾呼び出し (caml2html: emit_tailcall) *)
      g'_args oc [(x, reg_cl)] ys zs;
      (* クロージャを呼び出し *)
      Printf.fprintf oc "\tldr %s, [%s, 0]\n" (reg reg_sw) (reg reg_cl);
      Printf.fprintf oc "\tbr %s\n" (reg reg_sw)
      (* Printf.fprintf oc "\tlwz\t%s, 0(%s)\n" (reg reg_sw) (reg reg_cl);
      Printf.fprintf oc "\tmtctr\t%s\n\tbctr\n" (reg reg_sw); *)
  | Tail, CallDir(Id.L(x), ys, zs) -> (* 末尾呼び出し *)
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tb\t%s\n" x
  | NonTail(a), CallCls(x, ys, zs) ->
      (* Printf.fprintf oc "\tmflr\t%s\n" (reg reg_tmp); *)
      g'_args oc [(x, reg_cl)] ys zs;
      let ss = stacksize () in
      (* lrをスタックへ退避 *)
      Printf.fprintf oc "\tmov %s, lr\n" (reg reg_tmp);
      Printf.fprintf oc "\tstr %s, [%s, %d]\n" (reg reg_tmp) (reg reg_sp) (ss - 8);
      Printf.fprintf oc "\tadd %s, %s, %d\n" (reg reg_sp) (reg reg_sp) ss;
      (* クロージャを呼び出し *)
      (* 参考: https://ie.u-ryukyu.ac.jp/~kono/compiler/c2/powerpc.html *)
      Printf.fprintf oc "\tldr %s, [%s, 0]\n" (reg reg_tmp) (reg reg_cl);
      Printf.fprintf oc "\tblr %s\n" (reg reg_tmp);
      Printf.fprintf oc "\tsub %s, %s, %d\n" (reg reg_sp) (reg reg_sp) ss;
      Printf.fprintf oc "\tldr %s, [%s, %d]\n" (reg reg_tmp) (reg reg_sp) (ss - 8);
      (* Printf.fprintf oc "\tstw\t%s, %d(%s)\n" (reg reg_tmp) (ss - 4) (reg reg_sp);
      Printf.fprintf oc "\t@@@addi\t%s, %s, %d\n" (reg reg_sp) (reg reg_sp) ss;
      Printf.fprintf oc "\tlwz\t%s, 0(%s)\n" (reg reg_tmp) (reg reg_cl);
      Printf.fprintf oc "\tmtctr\t%s\n" (reg reg_tmp);
      Printf.fprintf oc "\tbctrl\n";
      Printf.fprintf oc "\t@@@subi\t%s, %s, %d\n" (reg reg_sp) (reg reg_sp) ss;
      Printf.fprintf oc "\tlwz\t%s, %d(%s)\n" (reg reg_tmp) (ss - 4) (reg reg_sp); *)
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\tmov %s, %s\n" (reg a) (reg regs.(0))
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tfmr\t%s, %s\n" (reg a) (reg fregs.(0));
      (* lrをスタックから復元 *)
      Printf.fprintf oc "\tmov lr, %s\n" (reg reg_tmp)
      (* Printf.fprintf oc "\tmtlr\t%s\n" (reg reg_tmp) *)
  | (NonTail(a), CallDir(Id.L(x), ys, zs)) ->
      g'_args oc [] ys zs;
      let ss = stacksize () in
      (* lrをスタックへ退避 *)
      Printf.fprintf oc "\tmov %s, lr\n" (reg reg_tmp);
      Printf.fprintf oc "\tstr %s, [%s, %d]\n" (reg reg_tmp) (reg reg_sp) (ss - 8);
      Printf.fprintf oc "\tadd %s, %s, %d\n" (reg reg_sp) (reg reg_sp) ss;
      Printf.fprintf oc "\tbl %s\n" x;
      Printf.fprintf oc "\tsub %s, %s, %d\n" (reg reg_sp) (reg reg_sp) ss;
      Printf.fprintf oc "\tldr %s, [%s, %d]\n" (reg reg_tmp) (reg reg_sp) (ss - 8);
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\tmov %s, %s\n" (reg a) (reg regs.(0))
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tfmr\t%s, %s\n" (reg a) (reg fregs.(0));
      (* lrをスタックから復元 *)
      Printf.fprintf oc "\tmov lr, %s\n" (reg reg_tmp)
and g'_tail_if oc e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s %s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)
and g'_non_tail_if oc dest e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "\t%s %s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tb\t%s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_args oc x_reg_cl ys zs =
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "\tmov %s, %s\n" (reg r) (reg y))
    (shuffle reg_sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> Printf.fprintf oc "\tfmov %s, %s\n" (reg fr) (reg z))
    (shuffle reg_fsw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  if data <> [] then
    (Printf.fprintf oc "\t.data\n\t.literal8\n";
     List.iter
       (fun (Id.L(x), d) ->
         Printf.fprintf oc "\t.align 3\n";
         Printf.fprintf oc "%s:\t # %f\n" x d;
         Printf.fprintf oc "\t.long\t%ld\n" (gethi d);
         Printf.fprintf oc "\t.long\t%ld\n" (getlo d))
       data);
  Printf.fprintf oc "\t.text\n";
  Printf.fprintf oc "\t.globl _min_caml_start\n";
  Printf.fprintf oc "\t.align 2\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc "_min_caml_start: # main entry point\n";
  (* sp と hp を設定 *)
  Printf.fprintf oc "\tadd %s, %s, 0\n" (reg reg_sp) (reg "%x0");
  Printf.fprintf oc "\tadd %s, %s, 0\n" (reg reg_hp) (reg "%x1");
  (* fpとlrをスタックへ退避 *)
  Printf.fprintf oc "\tstr fp, [%s, 0]\n" (reg reg_sp);
  Printf.fprintf oc "\tstr lr, [%s, 8]\n" (reg reg_sp);
  Printf.fprintf oc "\tadd %s, %s, 16\n" (reg reg_sp) (reg reg_sp);

  Printf.fprintf oc "#\tmain program starts\n";
  stackset := S.empty;
  stackmap := [];
  g oc (NonTail("_R_0"), e);
  Printf.fprintf oc "#\tmain program ends\n";

  (* fpとlrをスタックから復元 *)
  Printf.fprintf oc "\tsub %s, %s, 16\n" (reg reg_sp) (reg reg_sp);
  Printf.fprintf oc "\tldr fp, [%s, 0]\n" (reg reg_sp);
  Printf.fprintf oc "\tstr lr, [%s, 8]\n" (reg reg_sp);
  (* 呼び出し元へ戻る *)
  Printf.fprintf oc "\tret\n"
