module type COLLECTION = sig
    type 'a coll
    val empty : 'a coll
    val peek : 'a coll -> 'a
    val push : 'a -> 'a coll -> 'a coll
    val pop : 'a coll -> 'a coll
end

module Queue : COLLECTION = struct
    type 'a coll = Queue of 'a list * 'a list

    let empty = Queue ([], [])

    let mk_queue = function
        | ([], ys) -> Queue (List.rev ys, [])
        | (xs, ys) -> Queue (xs, ys)

    let peek = function
        | Queue (x :: _, _) -> x
        | Queue ([], _) -> failwith "peek on empty queue"

    let push x (Queue (xs, ys)) = mk_queue (xs, x :: ys)

    let pop = function
        | Queue (_ :: xs, ys) -> mk_queue (xs, ys)
        | Queue ([], _) -> failwith "pop on empty queue"
end

module Stack : COLLECTION = struct
    type 'a coll = 'a list
    let empty = []
    let peek = function
        | [] -> failwith "peek on empty stack"
        | x :: _ -> x
    let pop = function
        | [] -> failwith "pop on empty stack"
        | _ :: xs -> xs
    let push x xs = x :: xs
end

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let ex_tree = Node (Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf)), 3, 
    Node (Leaf, 4, Node (Leaf, 5, Leaf)))

module Search = functor (M : COLLECTION) -> struct
    open M
    let search t =
        let rec it q xs =
            if q = empty
            then List.rev xs
            else match peek q with
                | Leaf -> it (pop q) xs
                | Node (l, v, r) -> it (q |> pop |> push l |> push r) (v :: xs)
        in it (push t empty) []
end

module Bfs = Search (Queue)
module Dfs = Search (Stack)

let bfs = Bfs.search
let dfs = Dfs.search

let () = 
    match bfs ex_tree with 
    | x :: _ -> print_int x
    | _ -> print_endline "hello"
;;

let () = 
    match dfs ex_tree with 
    | x :: _ -> print_int x
    | _ -> print_endline "hello"
;;

module type DICT = sig
    type ('a, 'b) dict
    val empty : ('a, 'b) dict
    val insert : 'a -> 'b -> ('a, 'b) dict -> ('a, 'b) dict
    val find_opt : 'a -> ('a, 'b) dict -> 'b option
    val find : 'a -> ('a, 'b) dict -> 'b
    val to_list : ('a, 'b) dict -> ('a * 'b) list
end

module ListDict : DICT = struct
    type ('a, 'b) dict = ('a * 'b) list
    let empty = []
    let remove k d = List.filter (fun (k', _) -> k <> k') d
    let insert k v d = (k, v) :: remove k d
    let find_opt k d = List.find_opt (fun (k', _) -> k = k') d |> Option.map snd
    let find k d = List.find (fun (k', _) -> k = k') d |> snd
    let to_list d = d
end

module type PRIO_QUEUE = sig
    type ('a, 'b) pq

    val empty : ('a, 'b) pq
    val insert : 'a -> 'b -> ('a, 'b) pq -> ('a, 'b) pq
    val pop : ('a, 'b) pq -> ('a, 'b) pq
    val min_with_prio : ('a, 'b) pq -> 'a * 'b
end

module ListPrioQueue : PRIO_QUEUE = struct
    type ('a, 'b) pq = ('a * 'b) list

    let empty = []
    let rec insert a x q = match q with
        | [] -> [a, x]
        | (b, y) :: ys -> if a < b then (a, x) :: q else (b, y) :: insert a x ys
    let pop q = List.tl q
    let min_with_prio q = List.hd q
end



(*===============================================================*)
(*===============================================================*)
(*===============================================================*)
(* KONIEC KODU Z WYKŁADU *)
(*===============================================================*)
(*===============================================================*)
(*===============================================================*)



(* zad 1 *)



module type HUFFMAN = sig 
    type 'a code_tree
    type 'a code_dict
    val code_tree : 'a list -> 'a code_tree
    val dict_of_code_tree : 'a code_tree -> 'a code_dict
    val encode : 'a list -> 'a code_dict -> int list
    val decode : int list -> 'a code_tree -> 'a list
end

module MakeHuffmanCode (Q: PRIO_QUEUE) (D: DICT) : HUFFMAN = struct
    type 'a code_tree = CTNode of 'a code_tree * 'a code_tree | CTLeaf of 'a
    type 'a code_dict = ('a, int list) D.dict

    let find_else x _ d =
        Option.value ~default:0 (D.find_opt x d)

    let freq_dict xs =
        let rec it xs d =
            match xs with
            | [] -> d
            | x :: xs' -> 
                it xs' (D.insert x (1 + find_else x 0 d) d)
        in it xs D.empty

    let initial_pq xs =
        List.fold_left (fun q (x, n) -> Q.insert n (CTLeaf x) q)
            Q.empty xs

    let rec algo q =
        let p1, t1 = Q.min_with_prio q
        and q1 = Q.pop q
        in if q1 = Q.empty then t1
        else let p2, t2 = Q.min_with_prio q1
            and q2 = Q.pop q1
            in algo (Q.insert (p1 + p2) (CTNode (t1, t2)) q2)

    let code_tree xs =
        xs |> freq_dict |> D.to_list |> initial_pq |> algo

    let dict_of_code_tree t =
        let rec aux t rcpref d =
            match t with
            | CTLeaf x -> D.insert x (List.rev rcpref) d
            | CTNode (l, r) -> aux l (0 :: rcpref) (aux r (1 :: rcpref) d)
        in aux t [] D.empty

    let encode xs d =
        List.fold_right (@) (List.map (fun x -> D.find x d) xs) []

    let decode bs t =
        let rec walk bs cur_t =
            match cur_t with
            | CTLeaf v -> v :: start bs
            | CTNode (l, r) ->
                match bs with
                | 0 :: bs' -> walk bs' l
                | 1 :: bs' -> walk bs' r
                | _ :: _ -> failwith "a value other than 0 or 1 encountered"
                | [] -> failwith "incomplete code"
        and start bs =
            if bs = [] then [] else walk bs t
        in start bs
end

module HuffmanCode = MakeHuffmanCode(ListPrioQueue)(ListDict)

let ex_string = "konstantynopolitańczykowianeczka"

let list_of_string s = String.to_seq s |> List.of_seq
let string_of_list s = List.to_seq s |> String.of_seq

let ex_code_tree = HuffmanCode.code_tree (list_of_string ex_string)
let ex_code_dict = HuffmanCode.dict_of_code_tree ex_code_tree
let ex_encoded_string = HuffmanCode.encode (list_of_string ex_string) ex_code_dict
let ex_decoded_list = HuffmanCode.decode ex_encoded_string ex_code_tree;;

print_endline "";;
print_endline "";;
List.iter (fun x -> print_int x) ex_encoded_string;
print_endline "";;
print_endline (string_of_list ex_decoded_list)



(* zad 2 *)



module type DICT = sig
    type ('a, 'b) dict
    val empty : ('a, 'b) dict
    val insert : 'a -> 'b -> ('a, 'b) dict -> ('a, 'b) dict
    val find_opt : 'a -> ('a, 'b) dict -> 'b option
    val find : 'a -> ('a, 'b) dict -> 'b
    val to_list : ('a, 'b) dict -> ('a * 'b) list
end

module type DICT = sig
    type key
    type 'a dict

    val empty : 'a dict
    val insert : key -> 'a -> 'a dict -> 'a dict
    val find_opt : key -> 'a dict -> 'a option
    val find : key -> 'a dict -> 'a
    val to_list : 'a dict -> (key * 'a) list
end



(* zad 3 *)



module MakeListDict (M: Map.OrderedType) : DICT = struct
    type key = M.t
    type 'a dict = (key * 'a) list

    let empty = []
    let remove k d = List.filter (fun (k', _) -> M.compare k k' <> 0) d
    let insert k v d = (k, v) :: remove k d
    let find_opt k d = List.find_opt (fun (k', _) -> M.compare k k' = 0) d |> Option.map snd
    let find k d = List.find (fun (k', _) -> M.compare k k' = 0) d |> snd
    let to_list d = d
end

module CharListDict = MakeListDict (Char)

(* let test = CharListDict.insert 'c' "balls" CharListDict.empty; //no work *) 

module MakeListDict (M : Map.OrderedType) : DICT with type key = M.t = struct
    type key = M.t
    type 'a dict = (key * 'a) list

    let empty = []
    let remove k d = List.filter (fun (k', _) -> M.compare k k' <> 0) d
    let insert k v d = (k, v) :: remove k d
    let find_opt k d = List.find_opt (fun (k', _) -> M.compare k k' = 0) d |> Option.map snd
    let find k d = List.find (fun (k', _) -> M.compare k k' = 0) d |> snd
    let to_list d = d
end

module CharListDict = MakeListDict (Char)

let test = CharListDict.insert 'c' "balls" CharListDict.empty;


(* zad 4 *)



module MakeMapDict (O: Map.OrderedType) : DICT with type key = O.t = struct
    module M = Map.Make(O)

    type key = O.t
    type 'a dict = 'a M.t

    let empty = M.empty
    let remove k d = M.remove k d 
    let insert k v d = M.add k v d 
    let find_opt k d = M.find_opt k d
    let find k d = M.find k d
    let to_list d = M.bindings d
end



(* zad 5 *)



module LeftistHeap = struct
    type ('a , 'b) heap =
        | HLeaf
        | HNode of int * ('a , 'b) heap * 'a * 'b * ('a , 'b) heap

    let rank = function HLeaf -> 0 | HNode (n , _ , _ , _ , _) -> n

    let heap_ordered p = function
        | HLeaf -> true
        | HNode (_ , _ , p', _ , _) -> p <= p'
    let rec is_valid = function
        | HLeaf -> true
        | HNode (n , l , p , _ , r) ->
            rank r <= rank l
            && rank r + 1 = n
            && heap_ordered p l
            && heap_ordered p r
            && is_valid l
            && is_valid r

    let make_node p v l r = 
        let rank_l = rank l and rank_r = rank r in 
        if rank_l >= rank_r then HNode (rank_r + 1, l, p, v, r)
        else HNode (rank_l + 1, r, p, v, l)

    let rec heap_merge h1 h2 = 
        match h1, h2 with 
        | HLeaf, _ -> h2
        | _, HLeaf -> h1
        | HNode (_, l1, p1, v1, r1), HNode (_, l2, p2, v2, r2) ->
            if p1 <= p2 then 
                let merged_right = heap_merge r1 h2 in 
                make_node p1 v1 l1 merged_right
            else 
                let merged_right = heap_merge r2 h1 in 
                make_node p2 v2 l2 merged_right
end



(* zad 6 *)




module type PRIO_QUEUE = sig
    type ('a, 'b) pq

    val empty : ('a, 'b) pq
    val insert : 'a -> 'b -> ('a, 'b) pq -> ('a, 'b) pq
    val pop : ('a, 'b) pq -> ('a, 'b) pq
    val min_with_prio : ('a, 'b) pq -> 'a * 'b
end

module HeapPrioQueue : PRIO_QUEUE = struct
    open LeftistHeap
    type ('a, 'b) pq = ('a, 'b) heap

    let empty = HLeaf
    let rec insert a x q = heap_merge q (make_node a x HLeaf HLeaf)
    let pop q = match q with
        | HLeaf -> HLeaf
        | HNode (_, l, _, _, r) -> heap_merge l r
    let min_with_prio q = match q with
        | HLeaf -> failwith "Queue is empty"
        | HNode (_, _, p, v, _) -> (p, v)
end

let pqsort xs = 
    let rec cons q xs = 
        match xs with 
        | [] -> q
        | x :: tl -> cons (HeapPrioQueue.insert x x q) tl
    and decons acc q =
        match q with 
        | HeapPrioQueue.empty -> acc
        | _ -> let mp, mv = HeapPrioQueue.min_with_prio q in 
            let qs = HeapPrioQueue.pop q in 
            decons (mv :: acc) qs in 
    let pq = cons HeapPrioQueue.empty xs in 
    decons [] pq
