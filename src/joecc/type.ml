type t =
  (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref
[@@deriving show]

let gentyp () = Var (ref None) (* 新しい型変数を作る *)

let rec print_t = function
  | Unit -> print_string "Unit"
  | Bool -> print_string "Bool"
  | Int -> print_string "Int"
  | Float -> print_string "Float"
  | Fun (ts, t) ->
    print_string "Fun(";
    List.iter
      (fun t ->
        print_t t;
        print_string ", ")
      ts;
    print_t t;
    print_string ")"
  | Tuple ts ->
    let count = ref 0 in
    print_string "Tuple(";
    List.iter
      (fun t ->
        if !count = List.length ts then print_t t else print_t t;
        print_string ", ";
        incr count)
      ts;
    print_string ")"
  | Array t ->
    print_string "Array(";
    print_t t;
    print_string ")"
  | Var opt_t_ref ->
    print_string "Var(";
    (match !opt_t_ref with Some v -> print_t v | None -> print_string "None");
    print_string ")"
;;
