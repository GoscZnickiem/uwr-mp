(* zad1 *)

let rec fib n = 
    if n = 0 then 0
    else if n = 1 then 1 
    else fib (n - 1) + fib (n - 2)
;;

let fib_iter n =
    let rec it n acc1 acc2 =
        if n = 0 then acc2
        else it (n-1) (acc1+acc2) acc1
    in it n 1 0
;;

let () = print_endline (" " ^ (string_of_int (fib 10)))
let () = print_endline (" " ^ (string_of_int (fib_iter 10)))



(* zad 2 *)

type mat = int * int * int * int

let new_mat a b c d = ((a, b, c, d) : mat)

let matrix_mult ((a1, b1, c1, d1) : mat) ((a2, b2, c2, d2) : mat) = 
    new_mat (a1 * a2 + b1 * c2) (a1 * b2 + b1 * d2)
            (c1 * a2 + d1 * c2) (c1 * b2 + d1 * d2)
;;
let matrix_id = new_mat 1 0 0 1

let matrix_expt m k = 
    if k = 0 then matrix_id 
    else let rec exp m k acc = 
        if k = 1 then acc
        else exp m (k-1) (matrix_mult m acc)
    in exp m k m
;;

let fib_matrix n =
    let (_, v, _, _) = matrix_expt (new_mat 1 1 1 0) n in v
;;

let () = print_endline (" " ^ (string_of_int (fib_matrix 10)))



(* zad 3 *)

let rec matrix_expt_fast m k =
    if k = 0 then matrix_id 
    else if k mod 2 = 0 then
        let half = matrix_expt_fast m (k / 2) in 
        matrix_mult half half
    else
        matrix_mult m (matrix_expt_fast m (k-1))
;;

let fib_fast n =
    let (_, v, _, _) = matrix_expt_fast (new_mat 1 1 1 0) n in v 
;;

let () = print_endline (" " ^ (string_of_int (fib_fast 10)))



(* zad 4 *)

let rec mem x xs =
    match xs with
    | [] -> false 
    | head :: tail -> head = x || mem x tail
;;

mem 4 [1; 2; 3; 4; 5];;
mem 4 [1; 2; 3; 6; 5];;



(* zad 5 *)

let maximum xs = 
    let rec max curr l = 
        match l with
        | [] -> curr 
        | head :: tail -> let u = if head > curr then head else curr in 
            max u tail 
    in max neg_infinity xs
;;

maximum [2.; 4.; 5.; 12.; 1.; 3.; 2.];;



(* zad 6 *)

let suffixes xs =
    let rec suf xs l =
        match xs with
        | [] -> [] :: l
        | _ :: tail ->
            suf tail (xs :: l)
    in suf xs []
;;

suffixes [2; 3; 7; 1];;



(* zad 7 *)

let is_sorted xs =
    match xs with
    | [] -> true
    | head :: tail -> 
            let rec f xs prev =
                match xs with
                | [] -> true
                | h :: t -> if h < prev then false else f t h 
            in f tail head
;;

is_sorted [1; 2; 3; 4];;
is_sorted [1; 5; 3; 4];;



(* zad 8 *)

let select xs = 
    let rec f x =
        match x with
        | [] -> failwith "pusta lista"
        | [x] -> (x, [])
        | head :: tail ->
                let (min, rest) = f tail in 
                if head < min then (head, tail) else (min, head :: rest)
    in f xs
;;

let rec select_sort xs =
    match xs with
    | [] -> []
    | [_] -> xs
    | _ -> let (a, rest) = select xs in 
        a :: select_sort rest
;;

select_sort [10; 8; 2; 8; 3; 2; 2; 10; 11; 3; 2; 9; 4];;



(* zad 9 *)

let split xs = 
    let rec f lst l1 l2 = 
        match lst with
        | [] -> (l1, l2)
        | [x] -> (x :: l1, l2)
        | x1 :: x2 :: tail -> f tail (x1 :: l1) (x2 :: l2)
    in f xs [] []
;;

let rec merge xs ys =
    match (xs, ys) with
    | ([], []) -> []
    | (x, []) -> x
    | ([], x) -> x
    | (x :: tx, y :: ty) ->
            if x > y then y :: merge (x :: tx) ty
            else x :: merge tx ( y :: ty)
;;

let rec merge_sort xs =
    match xs with
    | [] -> []
    | [x] -> [x]
    | x -> let (xa, xb) = split x in merge (merge_sort xa) (merge_sort xb)
;;

merge_sort [10; 8; 2; 8; 3; 2; 2; 10; 11; 3; 2; 9; 4];;



(* porównanie fibów *)

let () = print_endline "Porównanie:";;
let () = print_endline ((string_of_int (fib 40)))
let () = print_endline ((string_of_int (fib_iter 900000000)))
let () = print_endline ((string_of_int (fib_matrix 900000000)))
let () = print_endline ((string_of_int (fib_fast 900000000)))

let () = print_endline ((string_of_int (fib 20)))
let () = print_endline ((string_of_int (fib_iter 20)))
let () = print_endline ((string_of_int (fib_matrix 20)))
let () = print_endline ((string_of_int (fib_fast 20)))
