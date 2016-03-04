print_int 0; print_newline ();;

print_int (true && (let x = false in x) && true);;
