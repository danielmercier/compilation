print_int 23571023 ;;
print_newline () ;;

(* Calcul booléen. *)
if true  && false then print_int 1 else print_int 2 ;;
if true  && true  then print_int 3 else print_int 4 ;;
if true  || false then print_int 5 else print_int 6 ;;
if false || true  then print_int 7 else print_int 8 ;;
if false || false then print_int 9 else print_int 10 ;;
if not true       then print_int 1 else print_int 2 ;;
if not false      then print_int 3 else print_int 4 ;;
