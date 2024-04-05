let rec ack x y =
  if x = 0 then y + 1 else
  if y = 0 then ack (x-1) 1 else
  ack (x-1) (ack x (y-1)) in
let a = print_int(ack 1 1) in
let _ = print_int(a) in ()
