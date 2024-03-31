open MinCaml
open Asm
open Config
open Insts
module List = ListLabels

exception Error of string

(* generate a unique label id *)
let gen_label, reset =
  let counter = ref 0 in
  ( (fun () ->
      let l = !counter in
      counter := !counter + 1;
      "$" ^ string_of_int l)
  , fun () -> counter := 0 )
;;

(* compilation environment maps local variable names to local variable
   numbers *)
let lookup env var =
  match
    List.find_opt ~f:(fun (_, v) -> var = v) (List.mapi ~f:(fun idx v -> idx, v) env)
  with
  | Some v -> fst v
  | None -> failwith (Printf.sprintf "%s not found" var)
;;

let extend_env env var = var :: env
let shift_env env = extend_env env "*dummy*"
let downshift_env env = List.tl env
let return_address_marker = "$ret_addr"
let jit_flg_marker = "$jit_flg"

let build_arg_env args =
  match !sh_flg with
  | true -> return_address_marker :: jit_flg_marker :: List.rev args
  | false -> return_address_marker :: List.rev args
;;

(* computes the number of arguments to this frame. The stack has a shape like
   [...local vars...][ret addr][..args...], the return address position from the
   top indicates the number of local variables on top of the return address. *)
let arity_of_env env =
  let num_local_vars = lookup env return_address_marker in
  List.length env - num_local_vars - 1, num_local_vars
;;

let arity_of_env_sh env =
  let num_local_vars = lookup env return_address_marker in
  let flg_offset = lookup env return_address_marker in
  List.length env - num_local_vars - 1, num_local_vars, flg_offset
;;

let label_counter = ref 0

let gen_label _ =
  let l = !label_counter in
  label_counter := l + 1;
  "$" ^ string_of_int l
;;

let reset _ = label_counter := 0

let compile_id_or_imm env = function
  | Asm.C n -> if n = 0 then [ CONST0 ] else [ CONST; Literal n ]
  | Asm.V x ->
    let y = lookup env x in
    if y = 0 then [ DUP0 ] else [ DUP; Literal y ]
;;

let rec compile_t fname env =
  let open Asm in
  function
  | Ans (CallDir (Id.L fname', args, fargs) as e) ->
    if not @@ !Config.tail_opt_flg
    then compile_exp fname env e
    else if fname' = fname
    then (
      let old_arity, local_size = arity_of_env env in
      let new_arity = List.length args in
      (List.fold_left
         ~f:(fun (rev_code_list, env) v ->
           compile_id_or_imm env (V v) :: rev_code_list, shift_env env)
         ~init:([], env)
         args
      |> fst
      |> List.rev
      |> List.flatten)
      @ (if !Config.frame_reset_flg
        then
          [ FRAME_RESET
          ; Literal old_arity
          ; Literal local_size
          ; Literal new_arity
          ]
        else [])
      @ [ JUMP; Lref fname ])
    else compile_exp fname env e
  | Ans e -> compile_exp fname env e
  | Let ((x, _), exp, t) ->
    let ex_env = extend_env env x in
    compile_exp fname env exp @ compile_t fname ex_env t @ [ POP1 ]

and compile_exp fname env exp =
  let open Asm in
  match exp with
  | Nop -> []
  | Set i -> compile_id_or_imm env (C i)
  | Mov var -> compile_id_or_imm env (V var)
  | Neg var -> compile_id_or_imm env (V var) @ [ NEG ]
  | Add (x, y) ->
    compile_id_or_imm env (V x) @ compile_id_or_imm (shift_env env) y @ [ ADD ]
  | Sub (x, y) ->
    compile_id_or_imm env (V x) @ compile_id_or_imm (shift_env env) y @ [ SUB ]
  | Mul (x, y) ->
    compile_id_or_imm env (V x) @ compile_id_or_imm (shift_env env) y @ [ MUL ]
  | Div (x, y) ->
    compile_id_or_imm env (V x) @ compile_id_or_imm (shift_env env) y @ [ DIV ]
  | Mod (x, y) ->
    compile_id_or_imm env (V x) @ compile_id_or_imm (shift_env env) y @ [ MOD ]
  | IfEq (x, y, then_exp, else_exp) ->
    let l2, l1 = gen_label (), gen_label () in
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) y
    @ [ EQ ]
    @ [ JUMP_IF_ZERO; Lref l1 ]
    @ compile_t fname env then_exp
    @ [ JUMP; Lref l2 ]
    @ [ Ldef l1 ]
    @ compile_t fname env else_exp
    @ [ Ldef l2 ]
  | IfLE (x, y, then_exp, else_exp) ->
    let l2, l1 = gen_label (), gen_label () in
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) y
    @ [ LT ]
    @ [ JUMP_IF_ZERO; Lref l1 ]
    @ compile_t fname env then_exp
    @ [ JUMP; Lref l2 ]
    @ [ Ldef l1 ]
    @ compile_t fname env else_exp
    @ [ Ldef l2 ]
  | IfGE (x, y, then_exp, else_exp) ->
    let l2, l1 = gen_label (), gen_label () in
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) y
    @ [ GT ]
    @ [ JUMP_IF_ZERO; Lref l1 ]
    @ compile_t fname env then_exp
    @ [ JUMP; Lref l2 ]
    @ [ Ldef l1 ]
    @ compile_t fname env else_exp
    @ [ Ldef l2 ]
  | CallDir (Id.L "min_caml_print_int", [ x ], _) ->
    compile_id_or_imm env (V x) @ [ PRINT_INT ]
  | CallDir (Id.L "min_caml_print_newline", _, _) -> [ PRINT_NEWLINE ]
  | CallDir (Id.L "min_caml_read_int", _, _) -> [ READ_INT ]
  | CallDir (Id.L "min_caml_rand_int", [ x ], _) ->
    compile_id_or_imm env (V x) @ [ RAND_INT ]
  | CallDir (Id.L "min_caml_create_array", [ x; y ], _) ->
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) (V y)
    @ [ ARRAY_MAKE ]
  | CallDir (Id.L var, rands, _) ->
    (List.fold_left
       ~f:(fun (rev_code_list, env) v ->
         compile_id_or_imm env (V v) :: rev_code_list, shift_env env)
       ~init:([], env)
       rands
    |> fst
    |> List.rev
    |> List.flatten)
    @ (if fname = "main" then [ JIT_SETUP ] else [])
    @ [ CALL; Lref var; Literal (List.length rands) ]
  | Ld (x, y, _) ->
    compile_id_or_imm env (V x) @ compile_id_or_imm (shift_env env) y @ [ GET ]
  | St (x, y, z, _) ->
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) (V y)
    @ compile_id_or_imm (shift_env (shift_env env)) z
    @ [ PUT ]
  | exp -> failwith (Printf.sprintf "un matched pattern: %s" (Asm.show_exp exp))
;;

let rec assoc_tail_rec fname = function
  | Ans (CallDir (Id.L fname', args, fargs)) -> fname = fname'
  | Ans e -> assoc_tail_rec' fname e
  | Let (_, _, t) -> assoc_tail_rec fname t

and assoc_tail_rec' fname = function
  | IfEq (_, _, e1, e2)
  | IfLE (_, _, e1, e2)
  | IfGE (_, _, e1, e2)
  | IfFEq (_, _, e1, e2)
  | IfFLE (_, _, e1, e2) ->
    assoc_tail_rec fname e1 || assoc_tail_rec fname e2
  | e -> false
;;

(* resolving labels *)
let assoc_if subst elm = try List.assoc elm subst with Not_found -> elm

(* [...;Ldef a;...] -> [...;a,i;...] where i is the index of the next
   instruction of Ldef a in the list all Ldefs are removed e.g., [_;Ldef
   8;_;Ldef 7;_] ==> [8,1; 7,2] *)
let make_label_env instrs =
  snd
    (List.fold_left
       ~f:
         (fun (addr, env) -> function
           | Ldef n -> addr, (Lref n, Literal addr) :: env
           | _ -> addr + 1, env)
       ~init:(0, [])
       instrs)
;;

(* remove all Ldefs and replace Lrefs with Literals *)
let resolve_labels instrs =
  let lenv = make_label_env instrs in
  instrs
  |> List.map ~f:(assoc_if lenv)
  |> List.filter ~f:(function Ldef _ -> false | _ -> true)
;;

let compile_fun_body fenv name arity annot exp env =
  (match annot with
  | Some `TJ -> [ TRACING_COMP ]
  | Some `MJ -> [ METHOD_COMP ]
  | None -> [])
  @ [ Ldef name ]
  @ compile_t name env exp
  @ if name = "main" then [ HALT ] else [ RET; Literal arity ]
;;

let compile_fun
    (fenv : Id.l -> Asm.fundef)
    Asm.{ name = Id.L name; args; body; annot }
  =
  compile_fun_body fenv name (List.length args) annot body (build_arg_env args)
;;

let compile_funs fundefs =
  (* let fenv name = fst(List.find (fun (_,{name=n}) -> name=n)
   *                       (List.mapi (fun idx fdef -> (idx,fdef))
   *                          fundefs)) in *)
  let fenv name = List.find ~f:(fun Asm.{ name = n } -> n = name) fundefs in
  Array.of_list
    (resolve_labels (List.flatten (List.map ~f:(compile_fun fenv) fundefs)))
;;

let resolve_labels' instrs =
  List.fold_left ~init:(0, []) ~f:(fun (addr, env) -> function
    | Ldef n -> addr, (Lref n, Literal addr) :: env | _ -> addr + 1, env)
;;

let f (Asm.Prog (_, fundefs, main)) =
  let open Asm in
  let main =
    { name = Id.L "main"
    ; args = []
    ; fargs = []
    ; ret = Type.Int
    ; body = main
    ; annot = None
    }
  in
  compile_funs (main :: fundefs)
;;
