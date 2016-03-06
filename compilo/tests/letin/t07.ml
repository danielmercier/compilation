print_int 9; print_newline ();;

let x = 6;;

let y = 3 in
let z = 
    let x = y in x 
in
print_int (z + x);;
