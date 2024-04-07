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
    if instr = 0000000 then interp stack sp bytecode (pc+1)
    else if instr = 01 then let v2 = stack.(sp-1) in let v1 = stack.(sp-2) in stack.(sp-2) <- (v1+v2) ; interp stack (sp-1) bytecode (pc+1)
    else if instr = 02 then let v2 = stack.(sp-1) in let v1 = stack.(sp-2) in stack.(sp-2) <- (v1-v2) ; interp stack (sp-1) bytecode (pc+1)
    else if instr = 03 then let v2 = stack.(sp-1) in let v1 = stack.(sp-2) in stack.(sp-2) <- (mul v1 v2) ; interp stack (sp-1) bytecode (pc+1)
    else if instr = 04 then let v2 = stack.(sp-1) in let v1 = stack.(sp-2) in stack.(sp-2) <- (div v1 v2) ; interp stack (sp-1) bytecode (pc+1)
    else if instr = 05 then let v2 = stack.(sp-1) in let v1 = stack.(sp-2) in stack.(sp-2) <- (rem v1 v2) ; interp stack (sp-1) bytecode (pc+1)
    else if instr = 06 then let v1 = stack.(sp-1) in let n = (if v1 = 0 then 1 else 0) in stack.(sp-1) <- n; interp stack sp bytecode (pc+1)
    else if instr = 07 then let v1 = stack.(sp-1) in stack.(sp-1) <- (-v1); interp stack sp bytecode (pc+1)
    else if instr = 08 then let v2 = stack.(sp-1) in let v1 = stack.(sp-2) in let n = (if v1 < v2 then 1 else 0) in stack.(sp-2) <- n ; interp stack (sp-1) bytecode (pc+1)
    else if instr = 09 then let v1 = stack.(sp-1) in let v2 = stack.(sp-2) in let v = (if v1 > v2 then 1 else 0) in stack.(sp-2) <- v ; interp stack (sp-1) bytecode (pc+1)
    else if instr = 10 then let v1 = stack.(sp-1) in let v2 = stack.(sp-2) in let v = (if v1 = v2 then 1 else 0) in stack.(sp-2) <- v ; interp stack (sp-1) bytecode (pc+1)
    else if instr = 11 then stack.(sp-1)
    else if instr = 12 then let addr = bytecode.(pc+1) in let v = stack.(sp-1) in let sp2 = sp - 1 in if v = 0 then (interp stack sp2 bytecode addr) else interp stack sp2 bytecode (pc + 2)
    else if instr = 13 then let addr = bytecode.(pc+1) in interp stack sp bytecode addr
    else if instr = 14 then let addr = bytecode.(pc+1) in let rands = bytecode.(pc+2) in
                            if is_mj addr then (stack.(sp) <- 100; (* push jit flag *)
                               let sp2 = sp+2 in let r = interp stack sp2 bytecode addr in
                                stack.(sp-rands) <- r ; interp stack (sp-rands+1) bytecode (pc+3))
                            else (stack.(sp) <- 200 ; (* push jit flag *) stack.(sp+1) <- pc+3 ; interp stack (sp+2) bytecode addr)

    else if instr = 15 then let n = bytecode.(pc+1) in let v = stack.(sp-1) in
                            let addr = stack.(sp-2) in  (* sp: sp-3 *)
                            let mode = stack.(sp-3) in  (* sp: sp-3 *)
                            if mode = 200 then (* check jit flag *) (stack.(sp-n-3) <- v; (* sp: sp-3-n+1 = sp-2-n *)
                            let sp2 = sp-n-2 in interp stack sp2 bytecode addr) else v

    else if instr = 16 then let n = bytecode.(pc+1) in let v = stack.(sp-n-1) in stack.(sp) <- v ; interp stack (sp+1) bytecode (pc+2)
    else if instr = 17 then let v = stack.(sp-1) in stack.(sp) <- v ; interp stack (sp+1) bytecode (pc+1)
    else if instr = 18 then let v = stack.(sp-1) in let _ = stack.(sp-2) in stack.(sp-2) <- v ; interp stack (sp-1) bytecode (pc+1)
    else if instr = 19 then let _ = stack.(sp-1) in interp stack (sp-1) bytecode (pc+1)
    else if instr = 20 then (stack.(sp) <- 0 ; interp stack (sp+1) bytecode (pc+1))
    else if instr = 21 then let c = bytecode.(pc+1) in stack.(sp) <- c; interp stack (sp+1) bytecode (pc+2)
    else if instr = 22 then let n = stack.(sp-1) in let arr = cast_fIAI(stack.(sp-2)) in stack.(sp-2) <- arr.(n); interp stack (sp-1) bytecode (pc+1)
    else if instr = 23 then let i = stack.(sp-1) in let arr = cast_fIAI(stack.(sp-2)) in let n = stack.(sp-3) in arr.(i) <- n; stack.(sp-3) <- cast_fAII(arr); interp stack (sp-2) bytecode (pc+1)
    else if instr = 24 then let init = stack.(sp-1) in let size = stack.(sp-2) in let a = Array.make size init in stack.(sp-2) <- cast_fAII(a); interp stack (sp-1) bytecode (pc+1)

    else if instr = 25 then let o = bytecode.(pc+1) in let l = bytecode.(pc+2) in
                            let n = bytecode.(pc+3) in let ret = stack.(sp-n-l-1) in
                            let old_base = sp-n-l-o-1 in let new_base = sp-n in
                            let sp2 = frame_reset stack old_base new_base ret n 0 in interp stack sp2 bytecode (pc+4)

    else if instr = 26 then interp stack sp bytecode (pc+1)
    else if instr = 28 then let n = read_int () in stack.(sp) <- n ; interp stack (sp+1) bytecode (pc+1)
    else if instr = 30 then let v = stack.(sp-1) in print_int v ; interp stack (sp-1) bytecode (pc+1)
    else if instr = 31 then let _ = print_newline() in interp stack sp bytecode (pc+1)
(*    else if instr = 32 then let v = stack.(sp-1) in print_string v ; interp stack (sp-1) bytecode (pc+1) *)
    else -1000 in

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
