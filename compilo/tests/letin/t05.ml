print_int 42; print_newline ();;

print_int (30 + (let x = 2 in let y = x + 4 in y + 4) + 2);;
