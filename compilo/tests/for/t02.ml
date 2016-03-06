print_int 45678;; print_newline ();;

(* test avec l'incr de boucle dans la memoire *)
let x = 1 in
let y = 5 in
let z = 3 in
for i = x to y do
    print_int (i + z)
done;;
