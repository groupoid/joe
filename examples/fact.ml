let rec fact x = if x = 0 then 1 else x * (fact (x-1)) in
let z = read_int() in print_int (fact z);print_newline()
