open Ast
open Mips
open Format



(* Constantes pour la représentation des données. *)
let word_size   = 4
let data_size   = word_size

let not_implemented() = failwith "Not implemented"



(* Les fonctions [push], [peek] et [pop] sont là pour vous aider à manipuler
   la pile. *)
  
(* val push : register -> text 
  [push reg] place le contenu de [reg] au sommet de la pile.
  $sp pointe sur l'adresse de la dernière case occupée. *)
let push reg =
  sub sp sp oi word_size
  @@ sw reg areg (0, sp)

(* val peek : register -> text
  [peek reg] place la valeur en sommet de pile dans [reg] sans dépiler. *)
let peek reg =
  lw reg areg (data_size - 4, sp)

(* val pop : register -> text
  [pop reg] place la valeur en sommet de pile dans [reg] et dépile. *)
let pop reg =
  peek reg
  @@ add sp sp oi data_size

    
(* La fonction de compilation des expressions prend en argument :
   l'expression à compiler. *)
let rec compile_expr e =
  match e.expr with
    | Econst c -> 
      begin
        match c with
	    | Cunit       -> push zero
        | Cint i      -> 
          (* Si on voit un entier, on le met sur le sommet de la pile *)
          li a0 i
          @@ push a0
      end

    (* Pour l'affichage, on calcul la valeur de l'argument, puis on saute au
       fragment de code gérant l'affichage proprement dit. *)
    | Eprint_newline e ->
      let e_code = compile_expr e in
      (* Il faudrai faire du typage pour voir que e es bien unit
       * On peut aussi tester si e = Cunit *)
      e_code
      @@ jal "print_newline"
    | Eprint_int e ->
      let e_code = compile_expr e in
      e_code
      @@ jal "print_int"
    | Eunop (Uminus, e) ->
      let e_code = compile_expr e in
      e_code
      (* Ce code prend l'entier en sommet de pile et empile son oppose *)
      @@ pop a0
      @@ sub a0 zero oreg a0
      @@ push a0
    | Ebinop (op, e1, e2) ->
      (* On évalue d'abord ce qu'il y à a droite puis ce qu'il y a à gauche *)
      let e_code1 = compile_expr e2
      and e_code2 = compile_expr e1
      (* Fonction qui prend un operateur mips et applique l'operateur
       * aux deux entiers au sommet de la pile et empile le resultat *)
      and fop op =
        pop a0
        @@ pop a1
        @@ op a0 a0 oreg a1
        @@ push a0
      in 
      e_code1
      @@ e_code2
      (* On appel fop avec le bonne operateur selon le type somme *)
      @@ match op with
           | Badd -> fop add
           | Bsub -> fop sub
           | Bmul -> fop mul
           | Bdiv -> fop div
      
(* Les instructions sont calculées l'une après l'autre. *)
let rec compile_instr_list il =
  (* On parcourt la liste de gauche a droite en ecrivant
   * a chaque fois la nouvelle expressione en mips *)
  List.fold_left 
    (fun acc -> function
        | Icompute e -> acc @@ (compile_expr e))
    nop
    il

      
(* Fragments de code pour [print_int] et [print_newline]. *)
let built_ins () =
  label "print_newline"
  @@ li a0 10 (* 10 correspond au code ascii de \n *)
  @@ li v0 11 (* Appel systeme pour afficher un caratere *)
  @@ syscall
  @@ jr ra

  @@ label "print_int"
  @@ pop a0
  @@ li v0 1 (* Appel systeme pour afficher un entier *)
  @@ syscall
  @@ jr ra
    
(* La compilation du programme produit un code en trois parties :
   1/ Le code principal (label "main")
   2/ Une instruction de fin (label "end_exec")
   3/ Le code des fonctions d'affichage (built_ins_code)
*)
let compile_prog p =
  let main_code = compile_instr_list p in
  let built_ins_code = built_ins () in
  
  { text =
     comment "Code principal"
  @@ label "main"
  @@ main_code
       
  @@ comment "Fin"
  @@ label "end_exec"
  @@ li v0 10
  @@ syscall
       
  @@ comment "Primitives"
  @@ built_ins_code
  ;
    
    data = nop
  }
