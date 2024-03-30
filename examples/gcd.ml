let rec gcd m n =
  if m <= 2 then n else
  if m <= n then gcd m (n-m) else
  gcd n (m - n) in
print_int (gcd 1230 262728293)

