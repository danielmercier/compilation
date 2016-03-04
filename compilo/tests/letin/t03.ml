print_int 8 ;;
print_newline () ;;

(* Variables locales emboîtées. *)
let x = let y = 3
	in y + 5
in print_int x
;;
