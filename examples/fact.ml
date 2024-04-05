let rec fact x =
  if x <= 1 then 1 else
    x * fact (x-1) in
let z = 5 in
let r = fact z in
print_int r
