print_int 15 ;;
print_newline () ;;

(* Variables locales emboîtées. *)
let x = let y = 3
	in y + 5
in
let y = x + 7 in
print_int y
;;
