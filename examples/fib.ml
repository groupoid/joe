let rec fib n = if n <= 1 then 1 else fib(n-1) + fib(n-2) in let a = fib 4 in print_int(a);print_newline()