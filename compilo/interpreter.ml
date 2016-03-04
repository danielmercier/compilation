open Ast
open Error

type value =
  | Vint    of int
  | Vbool   of bool
  | Vunit

module Env = Map.Make(String)

let get_int pos = function
  | Vint i -> i
  | _      -> error Interpretation_error pos

let get_bool pos = function
  | Vbool b -> b
  | _       -> error Interpretation_error pos

let rec interpret_expr env e =
  match e.node with
    | Econst c -> begin
      match c with
	| Cint i  -> Vint i
	| Cbool b -> Vbool b
	| Cunit   -> Vunit
    end

    | Eunop (Unot, e)   ->
      let b = get_bool e.info (interpret_expr env e) in Vbool (not b)
    | Eunop (Uminus, e) ->
      let i = get_int  e.info (interpret_expr env e) in Vint  (-i)

    | Ebinop ((Beq | Bneq) as op, e1, e2) ->
      let v1 = interpret_expr env e1
      and v2 = interpret_expr env e2
      in
      let b = match op with
	| Beq  -> v1 =  v2
	| Bneq -> v1 <> v2
	| _    -> assert false
      in
      Vbool b
	
    | Ebinop ((Blt | Ble | Bgt | Bge) as op, e1, e2) ->
      let i1 = get_int e1.info (interpret_expr env e1)
      and i2 = get_int e2.info (interpret_expr env e2)
      in
      let b = match op with
	| Blt -> i1 <  i2
	| Ble -> i1 <= i2
	| Bgt -> i1 >  i2
	| Bge -> i1 >= i2
	| _   -> assert false
      in
      Vbool b
	
    | Ebinop ((Badd | Bsub | Bmul | Bdiv) as op, e1, e2) -> 
      let i1 = get_int e1.info (interpret_expr env e1)
      and i2 = get_int e2.info (interpret_expr env e2)
      in
      let i = match op with
	| Badd -> i1 + i2
	| Bsub -> i1 - i2
	| Bmul -> i1 * i2
	| Bdiv -> i1 / i2
	| _    -> assert false
      in
      Vint i
	
    | Ebinop ((Band | Bor) as op, e1, e2) ->
      let b1 = get_bool e1.info (interpret_expr env e1) in
      let b = match op with
	| Band -> if b1 then get_bool e2.info (interpret_expr env e2) else false
	| Bor  -> if b1 then true else get_bool e2.info (interpret_expr env e2)
	| _    -> assert false
      in
      Vbool b

    | Eif (cond, e_then, e_else) ->
      let b = get_bool cond.info (interpret_expr env cond) in
      if b
      then interpret_expr env e_then
      else interpret_expr env e_else

    | Ewhile (cond, e) ->
      let b = ref (get_bool cond.info (interpret_expr env cond)) in
      let res = ref Vunit in
      while !b do
        res := interpret_expr env e;
        b :=  (get_bool cond.info (interpret_expr env cond))
      done;
      !res

    | Eprint_int e ->
      let i = get_int e.info (interpret_expr env e) in
      print_int i;
      Vunit

    | Eprint_newline e ->
      let _ = interpret_expr env e in
      print_newline ();
      Vunit

    | Eseq (el, last) ->
      List.iter (fun e -> ignore (interpret_expr env e)) el;
      interpret_expr env last
      
let interpret_prog p =
  let rec interpret_prog_aux env = function
    | [] -> print_newline()
      
    | Icompute e :: dl ->
      let _ = interpret_expr env e in
      interpret_prog_aux env dl

  in
  interpret_prog_aux Env.empty p
