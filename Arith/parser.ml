type token =
  | CONST_INT of (int)
  | CONST_UNIT
  | EOF
  | EOI
  | LPAREN
  | MINUS
  | PLUS
  | PRINT_INT
  | PRINT_NEWLINE
  | RPAREN
  | SLASH
  | STAR
  | UMINUS

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

  open Ast

  (* Fonction pour aider à la localisation des erreurs. *)
  let current_pos () =
    Parsing.symbol_start_pos (),
    Parsing.symbol_end_pos ()

  (* Fonction de construction d'un [node_expr], qui renseigne correctement
     la localisation de l'expression. *)
  let mk_node e = { expr = e; pos = current_pos() }

# 32 "parser.ml"
let yytransl_const = [|
  258 (* CONST_UNIT *);
    0 (* EOF *);
  259 (* EOI *);
  260 (* LPAREN *);
  261 (* MINUS *);
  262 (* PLUS *);
  263 (* PRINT_INT *);
  264 (* PRINT_NEWLINE *);
  265 (* RPAREN *);
  266 (* SLASH *);
  267 (* STAR *);
  268 (* UMINUS *);
    0|]

let yytransl_block = [|
  257 (* CONST_INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\001\000\002\000\002\000\003\000\
\003\000\003\000\003\000\002\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\014\000\013\000\000\000\000\000\000\000\000\000\
\016\000\000\000\000\000\000\000\005\000\000\000\012\000\007\000\
\006\000\001\000\003\000\004\000\000\000\000\000\000\000\000\000\
\015\000\000\000\000\000\011\000\010\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\012\000\013\000"

let yysindex = "\002\000\
\019\255\000\000\000\000\000\000\019\255\019\255\036\255\036\255\
\000\000\006\000\019\255\255\254\000\000\002\255\000\000\000\000\
\000\000\000\000\000\000\000\000\019\255\019\255\019\255\019\255\
\000\000\004\255\004\255\000\000\000\000"

let yyrindex = "\000\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\255\030\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\014\000\000\000\251\255\034\000"

let yytablesize = 42
let yytable = "\014\000\
\015\000\020\000\001\000\021\000\022\000\018\000\021\000\022\000\
\023\000\024\000\025\000\023\000\024\000\023\000\024\000\026\000\
\027\000\028\000\029\000\003\000\004\000\002\000\005\000\006\000\
\019\000\007\000\008\000\009\000\000\000\009\000\009\000\000\000\
\008\000\009\000\008\000\008\000\003\000\004\000\008\000\005\000\
\016\000\017\000"

let yycheck = "\005\000\
\006\000\003\001\001\000\005\001\006\001\000\000\005\001\006\001\
\010\001\011\001\009\001\010\001\011\001\010\001\011\001\021\000\
\022\000\023\000\024\000\001\001\002\001\000\000\004\001\005\001\
\011\000\007\001\008\001\003\001\255\255\005\001\006\001\255\255\
\003\001\009\001\005\001\006\001\001\001\002\001\009\001\004\001\
\007\000\008\000"

let yynames_const = "\
  CONST_UNIT\000\
  EOF\000\
  EOI\000\
  LPAREN\000\
  MINUS\000\
  PLUS\000\
  PRINT_INT\000\
  PRINT_NEWLINE\000\
  RPAREN\000\
  SLASH\000\
  STAR\000\
  UMINUS\000\
  "

let yynames_block = "\
  CONST_INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr_seq) in
    Obj.repr(
# 59 "parser.mly"
                ( _1 )
# 127 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
    ( [] )
# 133 "parser.ml"
               : 'instr_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instr_seq) in
    Obj.repr(
# 66 "parser.mly"
    ( _1 :: _2 )
# 141 "parser.ml"
               : 'instr_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
    ( Icompute _1 )
# 148 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 90 "parser.mly"
    ( _1 )
# 155 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 92 "parser.mly"
    ( mk_node (Eprint_newline _2) )
# 162 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 94 "parser.mly"
    ( mk_node (Eprint_int _2) )
# 169 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
    ( mk_node (Ebinop (Badd, _1, _3)) )
# 177 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
    ( mk_node (Ebinop (Bsub, _1, _3)) )
# 185 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
    ( mk_node (Ebinop (Bmul, _1, _3)) )
# 193 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
    ( mk_node (Ebinop (Bdiv, _1, _3)) )
# 201 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
    ( mk_node (Eunop (Uminus, _2)) )
# 208 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
    ( mk_node (Econst Cunit) )
# 214 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "parser.mly"
    ( mk_node (Econst (Cint _1)) )
# 221 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
    ( _2 )
# 228 "parser.ml"
               : 'simple_expr))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
