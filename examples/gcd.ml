let rec gcd m n =
  if m < 1 then n else
  if m < n + 1 then gcd m (n-m) else
  gcd n (m - n) in
print_int (gcd 1230 262728293)

