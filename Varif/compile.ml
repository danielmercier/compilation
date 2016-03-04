open Ast
open Mips
open Format
open Error

let not_implemented() = failwith "Not implemented"

let new_label =
  let c = ref 0 in
  fun () -> incr c; sprintf "__label__%05i" !c

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

(* On load dans reg l'emplacement loc *)
let to_register loc reg =
    match loc with
    | Memory (offset, regfrom) ->
        let memloc = (offset, regfrom) in
        lw reg areg memloc
    | Register regfrom ->
        if regfrom = reg then
            nop
        else
            move reg regfrom

(* On met le registre reg a l'emplacement loc *)
let from_register reg loc =
    match loc with
    | Memory (offset, regfrom) ->
        let memloc = (offset, regfrom) in
        sw reg areg memloc
    | Register regto ->
        if regto = reg then
            nop
        else
            move regto reg

(* Place le nombre a l'emplacement loc, dans la mémoire a l'emplacement 
  memloc *)
let to_memory loc memloc =
    match loc with
    | Memory (offset, reg) ->
        let memloc2 = (offset, reg) in
        if offset = fst memloc && reg = snd memloc then
            nop
        else
            lw a0 areg memloc2
            @@ sw a0 areg memloc
    | Register reg -> 
        sw reg areg memloc

(* Place le nombre dans la mémoire a l'emplacement memloc,
  dans l'emplacement loc*)
let from_memory memloc loc  =
    match loc with
    | Memory (offset, reg) ->
        let memloc2 = (offset, reg) in
        if offset = fst memloc && reg = snd memloc then
            nop
        else
            lw a0 areg memloc
            @@ sw a0 areg memloc2
    | Register reg -> 
        lw reg areg memloc

(* Place le nombre a l'emplacement locfrom, à l'emplacement locto *)
let from_loc_to locfrom locto =
    match (locfrom, locto) with
    | (Memory (offset, reg), _) -> 
        let memloc = (offset, reg) in 
        from_memory memloc locto
    | (_, Memory (offset, reg)) -> 
        let memloc = (offset, reg) in
        to_memory locfrom memloc
    | (Register reg, _) -> from_register reg locto

let to_stack = function
    | Register reg -> 
        push reg
    | Memory (offset, reg) ->
        lw a0 areg (offset, reg)
        @@ push a0

let from_stack = function
    | Register reg -> 
        pop reg
    | Memory (offset, reg) ->
        pop a0
        @@ sw a0 areg (offset, reg)

(* si loc est un registre, retourne le registre, sinon retourne le registre
 * en argument *)
let is_reg reg = function
    | Register reg -> reg
    | _ -> reg

(* test si loc est un registre et renvoie le registre, sinon, renvoie reg,
  renvoie aussi le code a executer si loc n'est pas un registre*)
let reg_for_op reg = function
    | Register reg -> (reg, nop)
    | _ as loc-> (reg, to_register loc reg)


    
(* La fonction de compilation des expressions prend en argument :
   l'expression à compiler avec la localisations de l'expression. *)
let rec compile_expr expr =
  match expr.node with
    | Econst c -> 
      begin
        match c with
	    | Cunit       -> 
          from_register zero expr.info
        | Cint i      -> 
          (match expr.info with
          | Register reg -> 
            li reg i
          | Memory (offset, reg) ->
            li a0 i
            @@ sw a0 areg (offset, reg))
        | Cbool b ->
          let i = 
            if b
            then 1
            else 0
          in
          (match expr.info with
          | Register reg -> 
            li reg i
          | Memory (offset, reg) ->
            li a0 i
            @@ sw a0 areg (offset, reg))
      end

    | Eident id -> 
        (*rien a faire, expr contient déjà la localisation de id*)
        nop

    (* Pour l'affichage, on calcul la valeur de l'argument, puis on saute au
       fragment de code gérant l'affichage proprement dit. *)
    | Eprint_newline e ->
      let e_code = compile_expr e in
      (* Il faudrai faire du typage pour voir que e es bien unit
       * On peut aussi tester si e = Cunit *)
      e_code
      @@ to_stack e.info
      @@ jal "print_newline"
      @@ from_stack expr.info

    | Eprint_int e ->
      let e_code = compile_expr e in
      e_code
      @@ to_stack e.info
      @@ jal "print_int"
      @@ from_stack expr.info

    | Eunop (Uminus, e) ->
      let e_code = compile_expr e in
      let (reg, code) = reg_for_op a0 e.info in
      e_code
      @@ code
      @@ sub reg zero oreg reg
      @@ from_register reg expr.info

    | Eunop (Unot, e) ->
      let e_code = compile_expr e in
      let (reg, code) = reg_for_op a0 e.info in
      let regres = is_reg a0 expr.info in
      e_code
      (* Ce code prend l'entier a la bonne localisation et rend 1 - l'entier
       puisque 1 = true et 0 = false *)
      @@ code
      @@ li a1 1
      @@ sub regres a1 oreg reg
      @@ from_register regres expr.info

    | Ebinop (op, e1, e2) ->
      (*e_code1 est executer en second, il y a donc la valeur de e_code2
       sur la pile, on ne veut pas perdre cette valeur dans le cas où il
       y a un letin, on augmente donc la place de la prochaine variable
       local*)
      let e_code1 = compile_expr e1
      and e_code2 = compile_expr e2
      in
      (* On split le match de op selon l'opération que l'on veut faire *)
      (match op with
        | Badd | Bsub | Bmul | Bdiv as op ->
          (* Fonction qui prend un operateur mips et applique l'operateur
           * aux deux entiers aux bonnes localisation *)
          let fop op =
            let (reg1, code1) = reg_for_op a0 e1.info in
            let (reg2, code2) = reg_for_op a1 e2.info in
            let reg3 = is_reg a0 expr.info in
            code1
            @@ code2
            @@ op reg3 reg1 oreg reg2
            @@ from_register reg3 expr.info
          in 
          (* On évalue d'abord ce qu'il y à a droite
           puis ce qu'il y a à gauche *)
          e_code2
          @@ e_code1
          (* On appel fop avec le bonne operateur selon le type somme *)
          @@ (match op with
               | Badd -> fop add
               | Bsub -> fop sub
               | Bmul -> fop mul
               | Bdiv -> fop div
               | _ -> (*IMPOSSIBLE*) failwith "impossible")

        | Band ->
          let label1 = new_label ()
          and label2 = new_label () in
          let (reg1, code1) = reg_for_op a0 e1.info in
          let reg2 = is_reg a0 expr.info in
          e_code1
          (* On vérifie si le booleen a la localisation expr.info = 0
           dans ce cas, on passe de suite a la suite sans evaluer
           executer l'autre code *)
          @@ code1
          @@ beqz reg1 label1
          (* Si e_code1 est vrai, le res du && est celui de e_code2 *)
          @@ e_code2
          @@ from_loc_to e2.info expr.info
          @@ b label2
          @@ label label1
          @@ li reg2 0
          @@ from_register reg2 expr.info
          @@ label label2

        | Bor -> 
          let label1 = new_label ()
          and label2 = new_label () in
          let (reg1, code1) = reg_for_op a0 e1.info in
          let reg2 = is_reg a0 expr.info in
          e_code1
          (* On vérifie si le booleen a la localisation expr.info = 1
           dans ce cas, on passe de suite a la suite sans evaluer
           executer l'autre code *)
          @@ code1
          @@ bnez reg1 label1
          (* Si e_code1 est faux, le res du || est celui de e_code2 *)
          @@ e_code2
          @@ from_loc_to e2.info expr.info
          @@ b label2
          @@ label label1
          @@ li reg2 1
          @@ from_register reg2 expr.info
          @@ label label2

        | Blt | Ble | Bgt | Bge | Beq | Bneq as op ->
          let label1 = new_label ()
          and label2 = new_label () in
          (* cette fonction fait un branchement selon la comparaison op *)
          let fop op =
            let (reg1, code1) = reg_for_op a0 e1.info in
            let (reg2, code2) = reg_for_op a1 e2.info in
            let regres = is_reg a0 expr.info in
            code1
            @@ code2
            (* On fait la soustraction pour comparer a zéro *)
            @@ sub a0 reg1 oreg reg2
            (* On compare avec op et on branche si vrai *)
            @@ op a0 label1
            (* Si faux on met 0 *)
            @@ li regres 0
            @@ from_register regres expr.info
            @@ b label2
            @@ label label1
            (* Si vrai on met 1 *)
            @@ li regres 1
            @@ from_register regres expr.info
            @@ label label2
          in
          e_code1
          @@ e_code2
          (* Selon op on appel fop avec le bonne opérateur de branchement *)
          @@ (match op with
                | Blt -> fop bltz
                | Ble -> fop blez
                | Bgt -> fop bgtz
                | Bge -> fop bgez
                | Beq -> fop beqz
                | Bneq -> fop bnez
                | _ -> (*IMPOSSIBLE*) failwith "impossible")
      )

    | Eif (cond, etrue, efalse) ->
      let cond_code = compile_expr cond
      and etrue_code = compile_expr etrue
      and efalse_code = compile_expr efalse
      and label1 = new_label ()
      and label2 = new_label () in
      let (regcond, codecond) = reg_for_op a0 cond.info in
      (* On execute le code de la condition *)
      cond_code
      (* On test si la condition est egal a zero, le cas echeant on jump
       au code qui correspond au false *)
      @@ codecond
      @@ beqz regcond label1
      @@ etrue_code
      @@ from_loc_to etrue.info expr.info
      (*on jump après le code correspondant au else*)
      @@ b label2
      @@ label label1
      @@ efalse_code
      @@ from_loc_to efalse.info expr.info
      @@ label label2

    | Ewhile (cond, e) ->
      let cond_code = compile_expr cond
      and e_code = compile_expr e
      and label1 = new_label ()
      and label2 = new_label () in
      let (reg, code) = reg_for_op a0 cond.info in
      label label1
      (* On execute le code de la condition *)
      @@ cond_code
      (* On test si la condition est egal a 0, le cas echeant on jump
       a la fin de la boucle *) 
      @@ code
      @@ beqz reg label2
      (* si pas egal zero alors on peu executer l'interieur de la boucle *)
      @@ e_code
      (* On jump au debut pour retester *)
      @@ b label1
      @@ label label2

      (*toute expression doit renvoyer quelquechose, while renvoie unit,
       on met zero *)
      @@ from_register zero expr.info

    | Eseq (liste, elast) ->
      List.fold_left
        (fun acc expr ->
          let e_code = compile_expr expr in
          acc @@ e_code)
        nop
        liste
      @@ let elast_code = compile_expr elast in
         elast_code
      @@ from_loc_to elast.info expr.info

    | Eletin (var, e1, e2) ->
      let e_code1 = compile_expr e1 in
      let e_code2 = compile_expr e2 in
      e_code1

      (* On peut maintenant exécuter le code 2, la variable local est
        placée *)
      @@ e_code2
    
      (* On stock la valeur de e_code2 dans un registre pour replacer
       sp *)
      @@ from_loc_to e2.info expr.info

(* Les instructions sont calculées l'une après l'autre. *)
let rec compile_instr_list nb_glob nb_loc il =
  (* On commence par allouer gp et sp avec nb_glob variables global *)
  let alloc = 
    sub gp sp oi (data_size * nb_glob)
    @@ sub sp gp oi (data_size * nb_loc)
    (* le sommet de pile n'est pas a gp mais a gp - data_size *)
    @@ sub fp gp oi data_size 
  in
  (* On parcourt la liste de gauche a droite en ecrivant
   * a chaque fois la nouvelle expressione en mips *)
  let code =
    List.fold_left 
      (* acc: l'accumulateur texte mips *)
      (fun acc -> function
              | Icompute e -> 
                 acc 
                 @@ (compile_expr e) 

              | Ilet (id, e) ->
                 let e_code = compile_expr e in
                 acc
                 (* puisque e contient la localisation de la variable,
                  elle sera bien placée *)
                 @@ e_code
      ) 
      alloc
      il
  in
  code
      
(* Fragments de code pour [print_int] et [print_newline]. *)
let built_ins () =
  label "print_newline"
  @@ pop zero
  @@ li a0 10 (* 10 correspond au code ascii de \n *)
  @@ li v0 11 (* Appel systeme pour afficher un caratere *)
  @@ syscall
  @@ push zero
  @@ jr ra

  @@ label "print_int"
  @@ pop a0
  @@ li v0 1 (* Appel systeme pour afficher un entier *)
  @@ syscall
  @@ push zero
  @@ jr ra
    
(* La compilation du programme produit un code en trois parties :
   1/ Le code principal (label "main")
   2/ Une instruction de fin (label "end_exec")
   3/ Le code des fonctions d'affichage (built_ins_code)
*)
let compile_prog p =
  let main_code = compile_instr_list 100 50 p in
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
