(* zad 4 *)

let func x y z =
    let f a b = a * a + b * b in
    if x > y then
        if y > z then
            f x y
        else
            f x z 
    else 
        if x > z then
            f x y
        else
            f y z
;;

(* zad 6 *)

test 0 (f());;
