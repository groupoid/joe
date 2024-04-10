open Printf
open MinCaml
open Config
open Insts
open Stdlib

let max_stack_depth = 100000

type fundef_bin_t = int array (* run the given program by calling the function id 0 *)
type fundef_asm_t = inst array (* convert the given program into binary, and then run *)

let with_debug f = match !vm_debug_flg with true -> f () | false -> ()

let index_of element array =
  fst (List.find
      (fun (_, v) -> v = element)
      (List.mapi (fun idx v -> idx, v) (Array.to_list array)))

let int_of_inst = function
  | Literal n -> n
  | Ldef lbl | Lref lbl -> failwith ("unresolved " ^ lbl)
  | inst -> index_of inst instsmap

let string_of = function
  | Literal n -> Printf.sprintf "Literal %d" n
  | Ldef n -> Printf.sprintf "Ldef %s" n
  | Lref n -> Printf.sprintf "Lref %s" n
  | i -> string_of_int (int_of_inst i)

(* operand stack

   We pair the stack pointer and the array of values. Though the reference to
   the arry is not changed over the push/pop operations, those operations return
   a new pair just for convenience. If we switch to the MinCaml implementation,
   we should decouple the pair. *)

type value =
  | Int' of int
  | String' of string
  | Array' of value array [@@deriving show]

type stack = int * value array
let push : stack -> value -> stack = fun (sp, stack) v -> stack.(sp) <- v ; sp + 1, stack
let pop : stack -> value * stack = fun (sp, stack) -> stack.(sp - 1), (sp - 1, stack)
let take : stack -> int -> value = fun (sp, stack) n -> stack.(sp - n - 1)
let drop : stack -> int -> stack = fun (sp, stack) n -> sp - n, stack

module Value = struct
  let ( |+| ) v1 v2 = match v1, v2 with | Int' i, Int' j -> Int' (i  +  j) | _ -> failwith "invalid value"
  let ( |-| ) v1 v2 = match v1, v2 with | Int' i, Int' j -> Int' (i  -  j) | _ -> failwith "invalid value"
  let ( |*| ) v1 v2 = match v1, v2 with | Int' i, Int' j -> Int' (i  *  j) | _ -> failwith "invalid value"
  let ( |/| ) v1 v2 = match v1, v2 with | Int' i, Int' j -> Int' (i  /  j) | _ -> failwith "invalid value"
  let ( |%| ) v1 v2 = match v1, v2 with | Int' i, Int' j -> Int' (i mod j) | _ -> failwith "invalid_value"
  let ( |<| ) v1 v2 = match v1, v2 with | Int' i, Int' j -> i < j          | _ -> failwith "invalid value"
  let ( |=| ) v1 v2 = match v1, v2 with | Int' i, Int' j -> i = j          | _ -> failwith "invalid value"

  let int_of_value = function
    | Int' i -> i
    | String' _ -> failwith "string is not int"
    | Array' _ -> failwith "array is not int"

  let string_of_value = function
    | Int' i -> failwith (sprintf "int %d is not string" i)
    | String' s -> s
    | Array' arr -> failwith (sprintf "array is not string")

  let array_of_value = function
    | Int' i -> failwith (sprintf "int %d is not array" i)
    | String' s -> failwith (sprintf "string %s is not array" s)
    | Array' arr -> arr

  let value_of_int i = Int' i
  let value_of_array arr = Array' arr
  let value_of_string s = String' s
end


let frame_reset : stack -> int -> int -> int -> stack =
 fun (sp, stack) o l n ->
  let ret = stack.(sp - n - l - 1) in
  (* save return address *)
  let jit_flg = stack.(sp - n - l - 2) in
  let old_base = sp - n - l - o - 1 in
  let new_base = sp - n in
  let _ =
    with_debug (fun () ->
        eprintf
          "offset: %d ret: %d, sp: %d, old_base: %d, new_base: %d\n"
          (sp - n - l - 1)
          (Value.int_of_value ret)
          sp
          old_base
          new_base)
  in
  let rec loop i =
    if n = i
    then (
      match !sh_flg with
      | true ->
        stack.(old_base + n) <- jit_flg;
        stack.(old_base + (n + 1)) <- ret;
        old_base + n + 2, stack
      | false ->
        stack.(old_base + n) <- ret;
        old_base + n + 1, stack)
    else (
      stack.(old_base + i) <- stack.(new_base + i);
      loop (i + 1))
  in
  loop 0

let make_stack () = 0, Array.make max_stack_depth (Int' 0)

(* let test_stack =(9,  [|1;2;3;4;5;6;7;8;9|])
 * let reset_result = frame_reset test_stack 2 2 3
 * let _ = Printf.printf "reset -> [%s]\n"
 *           (String.concat ";"
 *              (List.map string_of_int (Array.to_list (snd reset_result))))
 * let _ = assert  ((5, [|1;7;8;9;4;6;7;8;9|]) = reset_result) *)

(* fetch one integer from the code, and advance the program counter *)
let fetch code pc = code.(pc), pc + 1

let code_at_pc code pc =
    if 0 <= pc && pc < Array.length code
    then Printf.sprintf "code[%d..]=%d %d" pc code.(pc)
       ( if pc + 1 < Array.length code then code.(pc + 1) else -1 )
    else Printf.sprintf "pc=%d" pc

let dump_stack (sp, stack) =
    let rec loop i = if i = sp then "" else
      ( match stack.(i) with Int' i -> string_of_int i | Array' _ -> "array" | String' _ -> "string" )
      ^ ";" ^ loop (i + 1) in "[" ^ loop 0 ^ "]"

(* when the VM won't stop, you may turn on the following function to forcingly
   terminate after executing a certain amount of instructions *)
let checkpoint =
    if false then (let counter = ref 5000 in fun () ->
       if !counter = 0 then failwith "expired!" else counter := !counter - 1)
    else fun () -> ()

let debug pc inst stack = with_debug (fun () -> Printf.printf "%d %s %s\n" (pc - 1) (show_inst inst) (dump_stack stack))

let rec interp code pc stack =
  checkpoint ();
  let open Value in
  if pc < 0
  then
    fst (pop stack)
  else try
    let i, pc = fetch code pc in
    let _ = Printf.printf "%d:%d\n" pc i in
    let inst = instsmap.(i) in
    debug pc inst stack;
    match inst with
    | UNIT ->
      interp code (pc + 1) stack
    | NOT ->
      let v, stack = pop stack in
      let stack = if int_of_value v = 0 then push stack (Int' 1) else push stack (Int' 0) in
      interp code pc stack
    | NEG ->
      let v, stack = pop stack in
      let stack = push stack (v |*| Int' (-1)) in
      interp code pc stack
    | ADD ->
      let v2, stack = pop stack in
      let v1, stack = pop stack in
      let stack = push stack (v1 |+| v2) in
      interp code pc stack
    | SUB ->
      let v2, stack = pop stack in
      let v1, stack = pop stack in
      let stack = push stack (v1 |-| v2) in
      interp code pc stack
    | MUL ->
      let v2, stack = pop stack in
      let v1, stack = pop stack in
      let stack = push stack (v1 |*| v2) in
      interp code pc stack
    | MOD ->
      let v2, stack = pop stack in
      let v1, stack = pop stack in
      let stack = push stack (v1 |%| v2) in
      interp code pc stack
    | DIV ->
      let v2, stack = pop stack in
      let v1, stack = pop stack in
      let stack = push stack (v1 |/| v2) in
      interp code pc stack
    | LT ->
      let v2, stack = pop stack in
      let v1, stack = pop stack in
      let stack = push stack (if v1 |<| v2 then Int' 1 else Int' 0) in
      interp code pc stack
    | EQ ->
      let v2, stack = pop stack in
      let v1, stack = pop stack in
      let stack = push stack (if v1 |=| v2 then Int' 1 else Int' 0) in
      interp code pc stack
    | CONST ->
      let c, pc = fetch code pc in
      let stack = push stack (value_of_int c) in
      interp code pc stack
    | CONST0 ->
      let stack = push stack (value_of_int 0) in
      interp code pc stack
    | JUMP_IF_ZERO (* addr *) ->
      let addr, pc = fetch code pc in
      let v, stack = pop stack in
      (* interp code (if v=0 then addr else pc) stack *)
      if int_of_value v = 0
      then interp code addr stack
      else interp code pc stack
    | CALL (* addr argnum *) ->
      (* calling a function will create a new operand stack and lvars *)
      let addr, pc = fetch code pc in
      let _, pc = fetch code pc in
      (match !stack_mode_flg with
      | `User_stack ->
        let stack = Emit.(if !sh_flg then push stack (Int' 200) else stack) in
        let stack = push stack (value_of_int pc) in
        (* save return address *)
        (* (let (sp,s)=stack in
         *  if 2<sp then
         *    (Printf.printf "%d CALL %d [%d %d ...]\n" (pc-2) addr
         *       (s.(sp-2)) (s.(sp-3)))
         *  else ())
         * ; *)
        interp code addr stack
      | `Host_stack ->
        let stack = Emit.(if !sh_flg then push stack (Int' 100) else stack) in
        let v = interp code addr stack in
        let stack = push stack v in
        interp code pc stack)
    | RET (* n *) ->
      (* let pc0 = pc-1 in *)
      let n, pc = fetch code pc in
      let v, stack = pop stack in
      let _, stack = Emit.(if !sh_flg then pop stack else Int' 0, stack) in
      (match !stack_mode_flg with
      | `User_stack ->
        (* return value *)
        let pc, stack = pop stack in
        (* return address *)
        let stack = drop stack n in
        (* delete arguments *)
        let stack = push stack v in
        (* restore return value *)
        (* Printf.printf "%d RET with %d to %d\n" pc0 v pc; *)
        interp code (int_of_value pc) stack
      | `Host_stack -> v)
    | DUP ->
      let n, pc = fetch code pc in
      let stack = push stack (take stack n) in
      interp code pc stack
    | DUP0 ->
      let stack = push stack (take stack 0) in
      interp code pc stack
    | HALT -> fst (pop stack) (* just return the top value *)
    | FRAME_RESET (* o l n *) ->
      let o, pc = fetch code pc in
      let l, pc = fetch code pc in
      let n, pc = fetch code pc in
      let _ = Printf.printf "Frame Reset size: %d %d %d\n", o, l, n in
      let _ = with_debug (fun _ -> eprintf "o: %d, l %d, n: %d\n" o l n) in
      let stack = frame_reset stack o l n in
      interp code pc stack
    | POP1 ->
      let v, stack = pop stack in
      let _, stack = pop stack in
      let stack = push stack v in
      interp code pc stack
    | JUMP (* addr *) ->
      let n, _ = fetch code pc in
      interp code n stack
    | ARRAY_MAKE ->
      let init, stack = pop stack in
      let size, stack = pop stack in
      let stack = push stack (value_of_array (Array.make (int_of_value size) init)) in
      Printf.printf "ARRAY_MAKE size: 1\n" ; interp code pc stack
    | GET ->
      let n, stack = pop stack in
      let arr, stack = pop stack in
      let _ = Printf.printf "GET n: 1\n" in
      let stack = push stack (array_of_value arr).(int_of_value n) in
      interp code pc stack
    | PUT ->
      let i, stack = pop stack in
      let arr, stack = pop stack in
      let n, stack = pop stack in
      (array_of_value arr).(int_of_value i) <- n;
      let stack = push stack arr in
      interp code pc stack
    | PRINT_NEWLINE ->
      print_newline ();
      interp code pc stack
    | RAND_INT ->
      let n, stack = pop stack in
      let v = Random.int (int_of_value n) in
      let stack = push stack (value_of_int v) in
      interp code pc stack
    | READ_INT ->
      let v = read_int () in
      let stack = push stack (value_of_int v) in
      interp code pc stack
    | JIT_SETUP -> interp code pc stack
    | READ_STRING ->
      let v = read_line () in
      let stack = push stack (value_of_string v) in
      interp code pc stack
    | PRINT_INT ->
      let n, stack = pop stack in
      print_int (int_of_value n);
      let stack = push stack n in
      interp code pc stack
    | PRINT_STRING ->
      let s, stack = pop stack in
      let v = string_of_value s in print_string v;
      let stack = v |> String.length |> value_of_int |> push stack in
      interp code pc stack
    | _ -> failwith (sprintf "un matched pattern: %s" (show_inst inst))
  with | e -> raise e

let run_bin : fundef_bin_t -> int = fun fundefs ->
    let open Value in
    let stack = push (make_stack ()) (value_of_int (-987)) in
    int_of_value @@ interp fundefs 0 stack

let run_asm : fundef_asm_t -> int = fun fundefs -> run_bin (Array.map int_of_inst fundefs)
