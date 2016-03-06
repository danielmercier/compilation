open Ast
open Error
open Mips

module Env = Map.Make( String )

type var_loc =
    | Global_var of int
    | Local_var  of int

type venv = allocation Env.t

let mk_node node loc = { node = node; info = loc; }

let allocate_global n =
    Memory (n * data_size, gp)

let allocate_local = function
    | 0 -> Register t0
    | 1 -> Register t1
    | 2 -> Register t2
    | 3 -> Register t3
    | n -> Memory ((4 - n) * data_size, fp)

let rec allocation_expr nxt_local env expr =
    match expr.node with
    | Econst c ->
        mk_node (Econst c) (allocate_local nxt_local)

    | Eident id ->
        if not (Env.mem id env) then
            error (Unknown_identifier id) expr.info
        else
            mk_node (Eident id) (Env.find id env)

    | Eunop (op, e) ->
        let newe = allocation_expr nxt_local env e in
        mk_node (Eunop (op, newe)) (allocate_local nxt_local)

    | Ebinop (op, e1, e2) ->
        let newe1 = allocation_expr (nxt_local + 1) env e1 in
        let newe2 = allocation_expr nxt_local env e2 in
        mk_node (Ebinop (op, newe1, newe2)) (allocate_local nxt_local)

    | Eif (econd, ethen, eelse) ->
        let newecond = allocation_expr nxt_local env econd in
        let newethen = allocation_expr nxt_local env ethen in
        let neweelse = allocation_expr nxt_local env eelse in
        mk_node
            (Eif (newecond, newethen, neweelse))
            (allocate_local nxt_local)

    | Ewhile (econd, e) ->
        let newecond = allocation_expr nxt_local env econd in
        let newe = allocation_expr nxt_local env e in
        mk_node (Ewhile (newecond, newe)) (allocate_local nxt_local)

    | Efor (id, from, too, doo) ->
        let newefrom = allocation_expr nxt_local env from in
        let movefrom = mk_node (Expr newefrom) (allocate_local nxt_local) in
        let newetoo = allocation_expr (nxt_local + 1) env too in
        let newedoo =
            allocation_expr
                (nxt_local + 2)
                (Env.add id (allocate_local nxt_local) env)
                doo
        in
        mk_node
            (Efor (id, newefrom, newetoo, newedoo))
            (allocate_local nxt_local)

    | Eletin (id, eassign, e) ->
        let newassign = 
            allocation_expr nxt_local env eassign in
        let newe = 
            allocation_expr 
                (nxt_local + 1) 
                (Env.add id (allocate_local nxt_local) env)
                e
        in
        mk_node (Eletin (id, newassign, newe)) (allocate_local nxt_local)

    | Eseq (el, last) ->
        mk_node 
            (Eseq 
                ((List.map (fun e -> allocation_expr nxt_local env e) el),
                allocation_expr nxt_local env last))
            (allocate_local nxt_local)

    | Eprint_int e ->
        let newe = allocation_expr nxt_local env e in 
        mk_node (Eprint_int newe) (allocate_local nxt_local)

    | Eprint_newline e ->
        let newe = allocation_expr nxt_local env e in 
        mk_node (Eprint_newline newe) (allocate_local nxt_local)

let allocation_prog =
    let env = ref (Env.empty)
    and nxt_global = ref 0 in
    List.map
        (function
        | Icompute e ->
            let newe = allocation_expr 0 !env e in
            Icompute newe
        | Ilet (id, e) ->
            let { node = node; info = info } = 
                allocation_expr 0 !env e
            in
            let newe = 
                {node = node;
                 info = allocate_global !nxt_global;
                }
            in
            env := Env.add id (allocate_global !nxt_global) !env;
            incr nxt_global;
            Ilet (id, newe))
