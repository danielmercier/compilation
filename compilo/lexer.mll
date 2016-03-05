{

  open Lexing
  open Parser
  open Ast
  open Error
  open Format

  (* Petite fonction utile pour la localisation des erreurs. *)
  let current_pos b =
    lexeme_start_p b,
    lexeme_end_p b

  let keyword_table = Hashtbl.create 72
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
          [( "print_int", PRINT_INT);
           ("print_newline", PRINT_NEWLINE);
           ("if", IF);
           ("then", THEN);
           ("else", ELSE);
           ("true", TRUE);
           ("false", FALSE);
           ("not", NOT);
           ("while", WHILE);
           ("done", DONE);
           ("do", DO);
           ("let", LET);
           ("in", IN);
           ("for", FOR);
           ("to", TO)
          ]
}

let whitespace = [' ''\t']
let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z''A'-'Z']
let lower_alpha = ['a'-'z']
let ident = lower_alpha (alpha | '_' | digit)*

rule token = parse
  | whitespace      { token lexbuf } (* Les blancs, on rappel simplement la regle*)
  | ident as id
      {
        try
          Hashtbl.find keyword_table id
        with
          Not_found -> IDENT id
      }
  | "(*"            { comment (current_pos lexbuf) lexbuf; token lexbuf }
  | '\n'            { new_line lexbuf; token lexbuf } (* On appel new_line pour la localisation de la ligne de l'erreur*)
  | "()"            { CONST_UNIT }
  | number          { CONST_INT ( int_of_string (lexeme lexbuf) ) }
  | '='             { AFFECT }
  | "<="            { LE }
  | '<'             { LT }
  | ">="            { GE }
  | '>'             { GT }
  | "=="            { EQ }
  | "!="            { NEQ }
  | "&&"            { AND }
  | "||"            { OR }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { STAR }
  | '/'             { SLASH }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ";;"            { EOI }
  | ';'             { SEMICOLON }
  (* Fin du fichier. *)
  | eof             { EOF }
  | _               { error (Lexical_error (lexeme lexbuf)) (current_pos lexbuf) }

  (* commentaire imbrique grace aux appels de fonctions *)
and comment pos = parse
  | "*)"            { }
  | "(*"            { comment pos lexbuf; comment pos lexbuf } (* Commentaire imbrique *)
  | '\n'            { new_line lexbuf }
  | eof             { error (Lexical_error "comment opened but not closed") pos }
  | _               { comment pos lexbuf }
