print_int 0; print_int 101;;
print_newline ();;

let a = true && false;;
let b = true || (print_int 42; false);;
let c = a && (print_int b; b);;
let d = not a;;
print_int a; print_int b; print_int c; print_int d;;
