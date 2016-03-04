print_int 9 ;;
print_newline() ;;

(* Tests en cascade. *)
if true
then if false then print_int 5 else print_int 9
else if true then print_int 3 else print_int 4;;
