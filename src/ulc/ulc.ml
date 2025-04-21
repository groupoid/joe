type term =
  | Var of string
  | Lam of string * term
  | App of term * term
  | Pair of term * term
  | Fst of term
  | Snd of term
  | Unit

let rec string_of_term = function
  | Var x -> x
  | Lam (x, t) -> "λ" ^ x ^ ". " ^ string_of_term t
  | App (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | Pair (t1, t2) -> "<" ^ string_of_term t1 ^ ", " ^ string_of_term t2 ^ ">"
  | Fst t -> "fst " ^ string_of_term t
  | Snd t -> "snd " ^ string_of_term t
  | Unit -> "unit"

let rec subst x s = function
  | Var y -> if x = y then s else Var y
  | Lam (y, t) when x <> y -> Lam (y, subst x s t)
  | App (f, a) -> App (subst x s f, subst x s a)
  | Pair (t1, t2) -> Pair (subst x s t1, subst x s t2)
  | Fst t -> Fst (subst x s t)
  | Snd t -> Snd (subst x s t)
  | Unit -> Unit
  | t -> t

let rec equal t1 t2 =
  match t1, t2 with
  | Var x, Var y -> x = y
  | Lam (x, b), Lam (y, b') -> equal b (subst y (Var x) b')
  | Lam (x, b), t -> equal b (App (t, Var x))
  | t, Lam (x, b) -> equal (App (t, Var x)) b
  | App (f1, a1), App (f2, a2) -> equal f1 f2 && equal a1 a2
  | Pair (t1, t2), Pair (t1', t2') -> equal t1 t1' && equal t2 t2'
  | Fst t, Fst t' -> equal t t'
  | Snd t, Snd t' -> equal t t'
  | Unit, Unit -> true
  | _ -> false

let rec reduce = function
  | App (Lam (x, b), a) -> subst x a b
  | App (f, a) -> App (reduce f, reduce a)
  | Pair (t1, t2) -> Pair (reduce t1, reduce t2)
  | Fst (Pair (t1, t2)) -> t1
  | Fst t -> Fst (reduce t)
  | Snd (Pair (t1, t2)) -> t2
  | Snd t -> Snd (reduce t)
  | Unit -> Unit
  | t -> t

let rec normalize t =
  let t' = reduce t in
  if equal t t' then t else normalize t'

let id = Lam ("x", Var "x")
let const = Lam ("x", Lam ("y", Var "x"))
let one = Lam ("f", Lam ("x", App (Var "f", Var "x")))
let two = Lam ("f", Lam ("x", App (Var "f", App (Var "f", Var "x"))))

let beta = (App (Lam ("x", Var "x"), Var "y"), Var "y")
let eta = (Lam ("x", App (Var "f", Var "x")), Var "f")
let eta_domain = (Lam ("x", App (Var "f", Var "x")), Var "f")
let invalid_eta = (Lam ("x", Var "z"), Var "z")
let invalid_eta_v = (Lam ("x", Var "u"), Var "u")
let pair_test = (Fst (Pair (Var "x", Var "y")), Var "x")
let snd_test = (Snd (Pair (Var "x", Var "y")), Var "y")
let unit_test = (App (Lam ("x", Unit), Var "z"), Unit)

let test_equal name (t1, t2) =
  let t1' = normalize t1 in
  let t2' = normalize t2 in
  let result = equal t1' t2' in
  Printf.printf "Test %s:\n- Term1: %s\n- Term2: %s\n- Result: %s\n\n"
    name
    (string_of_term t1)
    (string_of_term t2)
    (if result then "PASS" else "FAIL")

let () =
  test_equal "Beta" beta;
  test_equal "Eta" eta;
  test_equal "Eta Domain" eta_domain;
  test_equal "Invalid Eta" invalid_eta;
  test_equal "Invalid Eta Var" invalid_eta_v;
  test_equal "Pair Fst" pair_test;
  test_equal "Pair Snd" snd_test;
  test_equal "Unit" unit_test;
