{

  open Lexing
  open Parser
  open Ast
  open Error

  (* Petite fonction utile pour la localisation des erreurs. *)
  let current_pos b =
    lexeme_start_p b,
    lexeme_end_p b

}

let whitespace = [' ''\t']
let digit = ['0'-'9']
let number = digit+

rule token = parse
  | whitespace { token lexbuf }
  (* commentaire *)
  | "(*" { comment (current_pos lexbuf) lexbuf; token lexbuf }
  (* Le retour à la ligne est traité à part pour aider la localisation. *)
  | '\n'
      { new_line lexbuf; token lexbuf }
      
  | "print_int"
      { PRINT_INT }
  | "print_newline"
      { PRINT_NEWLINE }
  | "()"
      { CONST_UNIT }
  | number { CONST_INT ( int_of_string (lexeme lexbuf) ) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ";;" { EOI }
      
  | _
      { error (Lexical_error (lexeme lexbuf)) (current_pos lexbuf) }
  (* Fin du fichier. *)
  | eof
      { EOF }

and comment pos = parse
  | "*)" { }
  | "(*" { comment pos lexbuf; comment pos lexbuf }
  | '\n' { new_line lexbuf }
  | eof { error (Lexical_error "comment opened but not closed") pos }
  | _ { comment pos lexbuf }
