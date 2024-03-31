let limit = ref 1000

let rec iter n e =
  Format.eprintf "iteration %d@." n;
  if n = 0
  then e
  else (
    let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
    if e = e' then e else iter (n - 1) e')
;;
