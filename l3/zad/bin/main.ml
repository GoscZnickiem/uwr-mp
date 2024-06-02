(* zad 1 *)

let fold_left f a xs =
    let rec it xs acc =
        match xs with
        | [] -> acc
        | x :: xs' -> it xs' (f acc x)
    in it xs a
;;

let product xs = fold_left ( * ) 
    1 xs
;;

(* zad 2 *)

let compose f g x = 
    f (g x)
;;

(* zad 3 *)

let build_list n f = 
    let rec it acc k f =
        if k = 0 then acc
        else it ((f (k-1)) :: acc) (k-1) f
    in it [] n f
;;

let negatives n = build_list n (fun x -> (-(x+1)));;

let reciprocals n = build_list n (fun x -> 1. /. float_of_int x);;

let evens n = build_list n (fun x -> 2 * x);;

let identityM n = build_list n
    (fun x -> build_list n (fun y -> if y = x then 1 else 0))
;;

(* zad 4 *)
