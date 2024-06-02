(* FUNKCJE REKURSYWNE *)

(* Funkcje rekursywne  wymagają słowa kluczowego rec. *)

let rec loop () = loop ();;

(* loop () -> loop () -> loop () -> ... *)

let rec fact n = 
    if n = 0 then 1
    else n * fact (n - 1)
;;

(* podejście iteracyjne jest jakieś rzymskie *)

let fact_iter n =
    let rec it n acc = 
        if n = 0 then acc
        else it (n - 1) (n * acc)
    in 
    it n 1
;;

(* fact_iter 5
-> it 5 1
-> if 5 = 0 then 1 else it (5 - 1) (5 * 1)
-> it (5 - 1) (5 * 1)
-> it 4 5
-> it (4 - 1) (4 * 5)
-> it 3 20
-> it (3 - 1) (3 * 20)
-> it 2 60
-> it (2 - 1) (2 * 60)
-> it 1 120
-> it 0 120
-> if 0 = 0 then 120 else it -1 0
-> 120 *)


(* REPREZENTACJA DANYCH *)

let t1 = (42, "foo");; (* krotka *)
let t2 = 23, "bar", true;;

fst t1;; (* 42 *)
snd t1;; (* "foo" *)

let a, b, c = t3;;

let fst_of_3 (a, b, c) = a;;
let fst_of_3 (a, _, _) = a;;
(* 'a * 'b * 'c -> 'a = <fun> *)

let snd_of_3 (_, a, _) = a;;
(* 'a * 'b * 'c -> 'b = <fun> *)



let o = Some 3;;
let on = None;;
(* konstruktory typu "option" *)

let val_of_option o b =
    match o with
    | Some a -> a 
    | None -> b
;;



type vec2 = float * float;;

let mk_vec2 x y = ((x, y) : vec2);;

let a, b = mk_vec2 (2., 3.);; (* a = 2.0, b = 3.0 *)

let vec_x (v : vec2) = fst v;; (* typ argumentu zawężony do vec2 *)
let vec_y (v : vec2) = snd v;;



let lista = [4; 8; 15; 16; 23; 42];;

let rec linf = 1 :: linf;; (* nieskończona lista 1 *)
(* :: dodaje element do listy *)

















