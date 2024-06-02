type expr =
  | Int of int
  | Add of expr * expr
  | Mult of expr * expr

let rec eval (e : expr) : int =
  match e with
  | Int n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Mult (e1, e2) -> eval e1 * eval e2

type rpn_cmd =
  | Push of int
  | RAdd
  | RMult

type rpn = rpn_cmd list

let rec to_rpn (e : expr) : rpn =
  match e with
  | Int n -> [Push n]
  | Add (e1, e2) -> to_rpn e1 @ to_rpn e2 @ [RAdd]
  | Mult (e1, e2) -> to_rpn e1 @ to_rpn e2 @ [RMult]

let rec eval_rpn (r : rpn) (s : int list) : int =
  match r, s with
  | [], [n] -> n
  | Push n :: r', _ -> eval_rpn r' (n :: s)
  | RAdd :: r', n1 :: n2 :: s' -> eval_rpn r' (n2 + n1 :: s')
  | RMult :: r', n1 :: n2 :: s' -> eval_rpn r' (n2 * n1 :: s')
  | _,_ -> failwith "error!"

(* ZAD 1 *)

(*
Indukcja:

Teza:
eval_rpn (to_rpn e @ rest) [X] = eval_rpn rest [eval e,X]

Baza (essa rigcz):
e = Int x
=> eval_rpn (to_rpn e @ rest) [X] =		(Def)
eval_rpn (Push x @ rest) [X] =			(Def)
eval_rpn rest (x :: [X]) =
eval_rpn rest [x, X] =					(Def)
eval_rpn rest [eval e, X]

tzn. eval_rpn (to_rpn e) [] = ([], [X]) -> X
takie, że X = eval e

Krok:
założenie indukcyjne: teza działa dla e1 i e2

e = Add (e1, e2)

to_rpn e = to_rpn e1 @ to_rpn e2 @ [RAdd]	(1)

eval_rpn (to_rpn e) [] =							(z 1)
eval_rpn (to_rpn e1 @ to_rpn e2 @ [RAdd]) [] =		(Z założenia indukcyjnego)
eval_rpn (to_rpn e2 @ [RAdd]) [X1] =				(Z założenia indukcyjnego)
eval_rpn ([Radd]) [X2, X1] =						(Def)
eval_rpn [] [X1 + X2] =								(Def)
X1 + X2 = eval e									(Def)

dla Mult podobnie
*)

(* ZAD 2 *)

let from_rpn (r : rpn) : expr =
    let rec from_rpn_s r s = 
        match r, s with
        | [], [x] -> x
        | Push n :: rs, _ -> from_rpn_s rs (Int n :: s)
        | RAdd :: rs, a :: b :: sx -> from_rpn_s rs (Add(a, b) :: sx )
        | RMult :: rs, a :: b :: sx -> from_rpn_s rs (Mult(a, b) :: sx )
        | _,_ -> failwith " error !"
    in from_rpn_s r []
  
(* ZAD 3 *)

(* porównaj zad 3. z listy 6. *)

let rec random_expr (max_depth : int) : expr =
    match Random.int (if max_depth > 1 then 3 else 1) with
    | 0 -> Int (Random.int 2137)
    | 1 -> Add ( random_expr (max_depth-1), random_expr (max_depth-1) )
    | 2 -> Mult ( random_expr (max_depth-1), random_expr (max_depth-1) )
    | _ -> failwith "impossible to get here"

let rec test (max_depth : int) (sample : int) : bool =
    if sample > 0 then
        let e = random_expr max_depth in
        if from_rpn (to_rpn e) = e then 
            test max_depth (sample - 1)
        else false
    else
        true

(* ZAD 4 *)

let rec test_ce (max_depth : int) (sample : int) : expr option =
    if sample > 0 then
        let e = random_expr max_depth in
        if from_rpn (to_rpn e) = e then 
            test_ce max_depth (sample - 1)
        else Some e
    else
        None
