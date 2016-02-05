open Ast
open Error

type value =
  | Vint    of int
  | Vunit

module Env = Map.Make(String)

let get_int pos = function
  | Vint i -> i
  | _      -> error Interpretation_error pos

let rec interpret_expr env e =
  match e.expr with
    | Econst c -> begin
      match c with
	| Cint i  -> Vint i
	| Cunit   -> Vunit
    end

    | Eunop (Uminus, e) ->
      let i = get_int  e.pos (interpret_expr env e) in Vint  (-i)
	
    | Ebinop (op, e1, e2) -> 
      let i1 = get_int e1.pos (interpret_expr env e1)
      and i2 = get_int e2.pos (interpret_expr env e2)
      in
      let i = match op with
	| Badd -> i1 + i2
	| Bsub -> i1 - i2
	| Bmul -> i1 * i2
	| Bdiv -> i1 / i2
      in
      Vint i
	
    | Eprint_int e ->
      let i = get_int e.pos (interpret_expr env e) in
      print_int i;
      Vunit

    | Eprint_newline e ->
      let _ = interpret_expr env e in
      print_newline ();
      Vunit

      
let interpret_prog p =
  let rec interpret_prog_aux env = function
    | [] -> print_newline()
      
    | Icompute e :: dl ->
      let _ = interpret_expr env e in
      interpret_prog_aux env dl

  in
  interpret_prog_aux Env.empty p
