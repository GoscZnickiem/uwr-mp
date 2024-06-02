(* zad 1 *)

let exists f xs = List.fold_left ( fun b x -> b || f x) false xs

exception Found
let exists f xs =
    try
        List.fold_left ( fun b x -> if f x then raise Found else b ) false xs
    with
    | Found -> true


(* zad 2 *)

let find (type t) p (xs : t list) = 
    let exception Found of t in
    try
        List.fold_left ( fun b x -> if p x then raise (Found x) else b ) () xs;
        raise Not_found
    with
    | Found x -> x 
