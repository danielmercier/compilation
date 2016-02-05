(* Position d'un élément de l'arbre de syntaxe abstraite. Utile pour rapporter
   les erreurs. *)
type position = Lexing.position * Lexing.position

(* On a deux types de constantes : les constantes entières et la valeur
   unité. *)
type const =
  | Cint of int
  | Cunit

(* Opérateurs unaires et binaires. *)
type unop = Uminus
type binop = Badd | Bsub | Bmul | Bdiv

(* Le type [node_expr] décrit un "nœud" formé d'une expression et d'une
   localisation dans le fichier source. *)
type node_expr = { expr: expr; pos: position }

(* Une expression est soit une constante, soit l'application d'une opération
   arithmétique, soit l'application d'une fonction d'affichage à une
   expression. *)
and expr =
  | Econst  of const
  | Eunop   of unop        * node_expr
  | Ebinop  of binop       * node_expr   * node_expr
  | Eprint_int of node_expr
  | Eprint_newline of node_expr

(* Un programme est une séquence d'instructions, chaque instruction étant
   simplement une expression. *)
type instr =
  | Icompute of node_expr
      
type prog = instr list
