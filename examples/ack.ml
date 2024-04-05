let rec ack m n =
    if m = 0 then n + 1 else
    if n = 0 then ack (m-1) 1 else ack (m-1) (ack m (n-1)) in
let res = ack 3 4 in
let _ = print_int res in
    print_newline()
