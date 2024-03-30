let rec is_mj _ = false in

let rec frame_reset stack old_base new_base ret n i =
  if n = i then (stack.(old_base + n) <- ret; old_base + n + 1)
  else (stack.(old_base + i) <- stack.(new_base + i);
        frame_reset stack old_base new_base ret n (i + 1)) in

let rec cast_fAII x = x in
let rec cast_fIAI x = x in

let rec frame_reset stack old_base new_base ret n i =
  if n = i then (stack.(old_base + n + 1) <- ret; old_base + n + 2)
  else (stack.(old_base + i) <- stack.(new_base + i);
        frame_reset stack old_base new_base ret n (i + 1)) in

let rec pop stack sp = stack.(sp - 1) in
let rec push stack sp v = stack.(sp) <- v in

let rec interp stack sp bytecode pc =
  let instr = bytecode.(pc) in
  (* print_debug pc instr sp; *)
  if instr = 0 then             (* UNIT *)
    interp stack sp bytecode (pc + 1)
  else if instr = 1 then        (* ADD *)
    let v2 = stack.(sp - 1) in  (* sp: sp - 1 *)
    let v1 = stack.(sp - 2) in  (* sp: sp - 2 *)
    stack.(sp-2) <- (v1+v2);    (* sp: sp - 1 *)
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 2 then        (* SUB *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- (v1 - v2);
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 3 then        (* MUL *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- (v1 * v2);
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 29 then       (* DIV *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- (v1 / v2);
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 30 then       (* MOD *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- (v1 mod v2);
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 4 then        (* NOT *)
    let v = stack.(sp - 1) in
    let n = (if v = 0 then 1 else 0)in
    stack.(sp - 1) <- n;
    interp stack sp bytecode (pc + 1)
  else if instr = 5 then        (* NEG *)
    let v = stack.(sp - 1) in
    stack.(sp - 1) <- (-v);
    interp stack sp bytecode (pc+1)
  else if instr = 6 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    let n = (if v1 < v2 then 1 else 0) in
    stack.(sp - 2) <- n;
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 7 then        (* EQ *)
    let v1 = stack.(sp - 1) in
    let v2 = stack.(sp - 2) in
    let v = (if v1 = v2 then 1 else 0) in
    stack.(sp - 2) <- v;
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 31 then        (* GT *)
    let v1 = stack.(sp - 1) in
    let v2 = stack.(sp - 2) in
    let v = (if v1 > v2 then 1 else 0) in
    stack.(sp - 2) <- v;
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 15 then       (* CONST *)
    let c = bytecode.(pc + 1) in
    stack.(sp) <- c;
    interp stack (sp + 1) bytecode (pc + 2)
  else if instr = 8 then        (* JUMP_IF_ZERO *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in
    let sp2 = sp - 1 in
    if v = 0 then (
      interp stack sp2 bytecode addr
    ) else
      interp stack sp2 bytecode (pc + 2)
  else if instr = 9 then        (* JUMP *)
    let addr = bytecode.(pc + 1) in
    interp stack sp bytecode addr
  else if instr = 10 then        (* CALL *)
    let addr = bytecode.(pc + 1) in
    let rands = bytecode.(pc + 2) in
    if is_mj addr then
      (stack.(sp) <- 100;       (* push jit flag *)
       let sp2 = sp+2 in
       let r = interp stack sp2 bytecode addr in
       stack.(sp - rands) <- r;
       interp stack (sp-rands+1) bytecode (pc+3))
    else
      (stack.(sp) <- 200;       (* push jit flag *)
       stack.(sp + 1) <- pc + 3;
       interp stack (sp + 2) bytecode addr)
  else if instr = 11 then       (* RET *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in
    let addr = stack.(sp-2) in  (* sp: sp-3 *)
    let mode = stack.(sp-3) in  (* sp: sp-3 *)
    if mode = 200 then          (* check jit flag *)
      (stack.(sp - n - 3) <- v; (* sp: sp-3-n+1 = sp-2-n *)
       let sp2 = sp - n - 2 in
       interp stack sp2 bytecode addr)
    else v
  else if instr = 12 then       (* HALT *)
    stack.(sp - 1)
  else if instr = 13 then       (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n - 1) in
    stack.(sp) <- v;
    interp stack (sp + 1) bytecode (pc + 2)
  else if instr = 14 then       (* POP1 *)
    let v = stack.(sp - 1) in
    let _ = stack.(sp - 2) in
    stack.(sp - 2) <- v;
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 16 then       (* GET *)
    let n = stack.(sp - 1) in
    let arr = cast_fIAI(stack.(sp - 2)) in
    stack.(sp - 2) <- arr.(n);
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 17 then       (* PUT *)
    let i = stack.(sp - 1) in
    let arr = cast_fIAI(stack.(sp - 2)) in
    let n = stack.(sp - 3) in
    arr.(i) <- n;
    stack.(sp - 3) <- cast_fAII(arr);
    interp stack (sp - 2) bytecode (pc + 1)
  else if instr = 18 then       (* ARRAYMAKE *)
    let init = stack.(sp - 1) in
    let size = stack.(sp - 2) in
    let a = Array.make size init in
    stack.(sp - 2) <- cast_fAII(a);
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 19 then       (* FRAME_RESET *)
    let o = bytecode.(pc + 1) in
    let l = bytecode.(pc + 2) in
    let n = bytecode.(pc + 3) in
    let ret = stack.(sp-n-l-1) in
    let old_base = sp - n - l - o - 1 in
    let new_base = sp - n in
    let sp2 = frame_reset stack old_base new_base ret n 0 in
    interp stack sp2 bytecode (pc + 4)
  else if instr = 20 then       (* PRINT_INT *)
    let v = stack.(sp - 1) in
    print_int v;
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 21 then       (* POP0 *)
    let _ = stack.(sp - 1) in
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 22 then       (* METHOD_ENTRY *)
    interp stack sp bytecode (pc + 1)
  else if instr = 23 then       (* CONST0 *)
    (stack.(sp) <- 0;
     interp stack (sp + 1) bytecode (pc + 1))
  else if instr = 24 then       (* DUP0 *)
    let v = stack.(sp - 1) in
    stack.(sp) <- v;
    interp stack (sp + 1) bytecode (pc + 1)
  else if instr = 25 then       (* METHOD_COMP *)
    interp stack sp bytecode (pc+1)
  else if instr = 26 then       (* TRACING_COMP *)
    interp stack sp bytecode (pc+1)
  else if instr = 27 then
    interp stack sp bytecode (pc+1)
  else if instr = 32 then
    let n = read_int () in
    stack.(sp) <- n;
    interp stack (sp+1) bytecode (pc+1)
  else
    -1000 in
let stk  = Array.make 200000 (-987) in
let rec read_code i n arr =
  if i = n then arr
  else
    (arr.(i) <- read_int ();
     read_code (i+1) n arr) in
let n = read_int () in
let arr = Array.make n 0 in
let code = read_code 0 n arr in
let _ = (interp stk 1 code 0) in
print_newline ()
