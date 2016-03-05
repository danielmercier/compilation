%{

  open Ast
  open Error

  (* Fonction pour aider à la localisation des erreurs. *)
  let current_pos () =
    Parsing.symbol_start_pos (),
    Parsing.symbol_end_pos ()

  (* Fonction de construction d'un [node_expr], qui renseigne correctement
     la localisation de l'expression. *)
  let mk_node e = { node = e; info = current_pos() }

%}


  
/* Liste des lexèmes, par ordre alphabétique. */
%token <int> CONST_INT
%token <string> IDENT
%token CONST_UNIT
%token EOF
%token EOI
%token SEMICOLON
%token PRINT_INT
%token PRINT_NEWLINE
%token LPAREN
%token RPAREN
%token MINUS PLUS SLASH STAR UMINUS
%token IF THEN ELSE
%token FOR TO WHILE DO DONE
%token TRUE FALSE
%token NOT AND OR
%token LT LE GT GE
%token EQ NEQ
%token LET IN
%token AFFECT

/*Precedence pour la règle let in*/
%nonassoc LET_IN

/*Token pour la precedence de IF THEN ELSE*/
%nonassoc IF_THEN_ELSE

/* Associativités et priorités. */
%left SEMICOLON
%left OR
%left AND
%left EQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%left SLASH STAR
%nonassoc UMINUS


/* Déclaration de la règle principale et de son type. */
%start prog
%type <Ast.parsed_prog> prog

%%



/* Début des règles d'analyse lexicale. */


/* Un programme est une séquence de zéro ou plusieurs instructions*/
prog:
| instr_seq EOF { $1 }

/* Si le parser ne produit pas la règle du dessus alors il y a une erreur.*/
| error         { error Syntax_error (current_pos ()) }
;

instr_seq:
| /* empty */   
    { [] }
| instr instr_seq      
    { $1 :: $2 }
;

/* Les identifiants */
ident:
    | IDENT { $1 }


/* Une instruction est une expression suivie par un ;; (EOI). */
instr:
| expr EOI
    { Icompute $1 }

/*Afféctation d'une variable global*/
| LET ident AFFECT expr EOI
    { Ilet ($2, $4) }
;
    
/* Les expressions sont de trois sortes :*/

/*1/ Des "expressions simples", qui sont soit des constantes soit des
  expressions délimitées par des parenthèses, soit des identifiants. */
simple_expr:
| CONST_UNIT
    { mk_node (Econst Cunit) }
| CONST_INT
    { mk_node (Econst (Cint $1)) }
| FALSE
    { mk_node (Econst (Cbool false)) }
| TRUE
    { mk_node (Econst (Cbool true)) }
| ident
    { mk_node (Eident $1) }
| LPAREN expr RPAREN
    { $2 }
;
expr:
| simple_expr
    { $1 }

/*2/ La combinaisons d'une ou plusieurs expressions par des opérateurs
  arithmétiques.*/
| expr PLUS expr
    { mk_node (Ebinop (Badd, $1, $3)) }
| expr MINUS expr
    { mk_node (Ebinop (Bsub, $1, $3)) }
| expr STAR expr
    { mk_node (Ebinop (Bmul, $1, $3)) }
| expr SLASH expr
    { mk_node (Ebinop (Bdiv, $1, $3)) }
| MINUS expr %prec UMINUS
    { mk_node (Eunop (Uminus, $2)) }

/*3/ L'application d'une fonction primitive [print_int] ou [print_newline]
  à une expression simple.*/
| PRINT_NEWLINE simple_expr
    { mk_node (Eprint_newline $2) }
| PRINT_INT simple_expr
    { mk_node (Eprint_int $2) }

/*Grammaire etendue pour les expressions booleennes*/
| NOT simple_expr
    { mk_node (Eunop (Unot, $2)) }
| expr AND expr
    { mk_node (Ebinop (Band, $1, $3)) }
| expr OR expr
    { mk_node (Ebinop (Bor, $1, $3)) }
| expr LT expr 
    { mk_node (Ebinop (Blt, $1, $3)) }
| expr LE expr 
    { mk_node (Ebinop (Ble, $1, $3)) }
| expr GT expr 
    { mk_node (Ebinop (Bgt, $1, $3)) }
| expr GE expr 
    { mk_node (Ebinop (Bge, $1, $3)) }
| expr EQ expr 
    { mk_node (Ebinop (Beq, $1, $3)) }
| expr NEQ expr 
    { mk_node (Ebinop (Bneq, $1, $3)) }
| IF expr THEN expr ELSE expr %prec IF_THEN_ELSE
    { mk_node (Eif ($2, $4, $6)) }
| LET ident AFFECT expr IN expr %prec LET_IN
    { mk_node (Eletin ($2, $4, $6)) }
    
/*Ajout du while*/
| WHILE expr DO expr DONE
    { mk_node (Ewhile ($2, $4)) }

/*Ajout de for*/
| FOR ident AFFECT expr TO expr DO expr DONE
    { mk_node (Efor ($2, $4, $6, $8)) }


/*Séparateur d'expressions*/
| expr SEMICOLON expr_seq { 
        let last::q = $3 in
        let l = $1::(List.rev q) in
        mk_node (Eseq (l, last))
    }
;

/*La derniere expression sera la premiere*/
expr_seq:
| expr %prec SEMICOLON
    { [$1] }
| expr_seq SEMICOLON expr
    { $3::$1 }
