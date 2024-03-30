type t =
  (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Mul of t * t
  | Div of t * t
  | Sub of t * t
  | Mod of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
  | List of t list
[@@deriving show]

and fundef =
  { name : Id.t * Type.t
  ; args : (Id.t * Type.t) list
  ; body : t
  ; mutable annot : [ `TJ | `MJ ] option
  }
[@@deriving show]

let rec print_t = function
  | Unit -> print_string "Unit"
  | Bool v -> Printf.printf "Bool(%s)" (string_of_bool v)
  | Int v -> Printf.printf "Int(%s)" (string_of_int v)
  | Float v -> Printf.printf "Float(%s)" (string_of_float v)
  | Not t ->
    print_string "Not(";
    print_t t;
    print_string ")"
  | Neg t ->
    print_string "Neg(";
    print_t t;
    print_string ")"
  | Add (t1, t2) ->
    print_string "Add(";
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ")";
    print_string ")"
  | Sub (t1, t2) ->
    print_string "Sub(";
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ")"
  | FNeg t ->
    print_string "FNeg(";
    print_t t;
    print_string ")"
  | FAdd (t1, t2) ->
    print_string "FAdd(";
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ")"
  | FSub (t1, t2) ->
    print_string "FSub(";
    print_t t1;
    print_t t2;
    print_string ")"
  | FMul (t1, t2) ->
    print_string "FMul(";
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ")"
  | FDiv (t1, t2) ->
    print_string "FDiv(";
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ")"
  | Eq (t1, t2) ->
    print_string "Eq(";
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ")"
  | LE (t1, t2) ->
    print_string "LE(";
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ")"
  | If (t1, t2, t3) ->
    print_string "If(";
    print_t t1;
    print_string ", ";
    print_newline ();
    print_t t2;
    print_string ", ";
    print_newline ();
    print_t t3;
    print_string ")"
  | Let ((id, typ), t1, t2) ->
    print_string "Let(";
    print_string "(";
    print_string id;
    print_string ",";
    Type.print_t typ;
    print_string "), ";
    print_t t1;
    print_string ", ";
    print_newline ();
    print_t t2;
    print_string ")"
  | Var id ->
    print_string "Var(";
    print_string id;
    print_string ")"
  | LetRec (fundef, t) ->
    print_string "LetRec(";
    print_fundef fundef;
    print_string ", ";
    print_t t;
    print_string ")"
  | App (t, ts) ->
    print_string "App(";
    print_t t;
    print_string ", ";
    print_t_list ts;
    print_string ")"
  | Tuple ts ->
    print_string "Tuple(";
    print_t_list ts;
    print_string ")"
  | LetTuple (args, t1, t2) ->
    let rec print_args args =
      match args with
      | [] -> ()
      | [ (id, typ) ] ->
        print_string id;
        print_string ", ";
        Type.print_t typ
      | (id, typ) :: tl ->
        print_string id;
        print_string ", ";
        Type.print_t typ;
        print_args tl
    in
    print_string "LetTuple(";
    print_string "(";
    print_args args;
    print_string ")";
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ")"
  | Array (t1, t2) ->
    print_string "Array(";
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ")"
  | Get (t1, t2) ->
    print_string "Get(";
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ")"
  | Put (t1, t2, t3) ->
    print_t t1;
    print_string ", ";
    print_t t2;
    print_string ", ";
    print_t t3;
    print_string ")"
  | _ -> assert false

and print_fundef { name; args; body } =
  let id, typ = name in
  print_string "{";
  print_string "(";
  print_string id;
  Type.print_t typ;
  print_string "); ";
  print_string "[";
  List.iter
    (fun arg ->
      let id, typ = arg in
      print_string id;
      print_string ", ";
      Type.print_t typ)
    args;
  print_t body;
  print_string "}"

and print_t_list ts =
  print_string "[";
  let rec loop ts =
    match ts with
    | [] -> ()
    | [ hd ] -> print_t hd
    | hd :: tl ->
      print_t hd;
      print_string "; ";
      loop tl
  in
  loop ts;
  print_string "]"
;;
