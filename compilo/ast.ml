
type const =
  | Cint of int
  | Cbool of bool
  | Cunit

type unop = Unot | Uminus
type binop =
  | Beq  | Bneq | Blt  | Ble  | Bgt  | Bge
  | Badd | Bsub | Bmul | Bdiv
  | Band | Bor

type ('info, 'data) node = { node: 'data; info: 'info }
type ident = string

type 'info expr = ('info, 'info expr_node) node
and 'info expr_node =
  | Econst  of const
  | Eident  of ident
  | Eunop   of unop        * 'info expr
  | Ebinop  of binop       * 'info expr   * 'info expr
  | Eif     of 'info expr  * 'info expr   * 'info expr
  | Ewhile  of 'info expr  * 'info expr
  | Efor    of ident       * 'info expr   * 'info expr   * 'info expr
  | Eletin  of ident * 'info expr   * 'info expr
  | Eseq    of 'info expr list * 'info expr
  | Eprint_int of 'info expr
  | Eprint_newline of 'info expr

  (* Une expression. Ce noeud sert dans l'allocation, si on veut changer la
   localisation d'une expression *)
  | Expr of 'info expr

type 'info instr =
  | Icompute of 'info expr
  | Ilet     of ident * 'info expr
      
type 'info prog = 'info instr list

type position = Lexing.position * Lexing.position
type parsed_prog = position prog

type allocation =
    (* En mémoire d'un décalage du registre *)
    | Memory of int * Mips.register

    (* Dans un registre *)
    | Register of Mips.register

type allocation_prog = allocation prog

(* Constantes pour la représentation des données. *)
let word_size   = 4
let data_size   = word_size
