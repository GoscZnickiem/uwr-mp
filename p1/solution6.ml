(* Terminologia: *)
(*     Pola białe i czarne (white and black) oznaczają  *)
(*     odpowiednio pola niezamalowane oraz zamalowane   *)

let ( let* ) xs ys = List.concat_map ys xs

(* 1) build_row *)

let sum l = List.fold_left (+) 0 l

let rec append_with v num xs = 
    if num = 0 then xs else
    append_with v (num - 1) (v :: xs)

let build_row spec size = 
    let whites = size - sum spec and groups = List.length spec in 
    if whites < (groups - 1) then [] else
    let rec create r spec groups whites last_white = 
        let w = if whites >= groups && whites != 0 then 
            create (false :: r) spec groups (whites - 1) true 
            else []
        and b = if last_white && groups != 0 then 
            create (append_with true (List.hd spec) r) (List.tl spec) (groups - 1) whites false 
            else []
        in if w = [] && b = [] then [r] else w @ b
    in create [] (List.rev spec) groups whites true

(* 2) build_candidate *)

let build_candidate spec size = 
    let rec f imgs spec = 
        match spec with 
        | [] -> imgs
        | x :: xs -> 
            let rows = build_row x size in
            f (let* img = imgs in List.map (fun row -> row :: img) rows) xs
    in f [[]] (List.rev spec)

(* 3) verify_row *)

let verify_row spec row = 
    let rec rc row = 
        match row with 
        | [] -> ([], 0)
        | true :: rs -> let recr = rc rs in (fst recr, snd recr + 1)
        | false :: rs -> let recr = rc rs in if snd recr <> 0 then ((snd recr)::(fst recr), 0) else recr
    in let pair = rc row
    in let recreated = if snd pair <> 0 then (snd pair)::(fst pair) else fst pair
    in recreated = spec

(* 4) verify_rows *)

let rec verify_rows spec img =
    match spec with 
    | [] -> true
    | x :: xs ->
        if verify_row x (List.hd img) then verify_rows xs (List.tl img) else false

(* 5) transpose *)

let rec transpose img =
    let rec join row ls =
        match row, ls with 
        | [], _ -> ls
        | _, [] -> List.map (fun x -> [x]) row
        | x :: xs, r :: rs -> (x::r) :: (join xs rs)
    in match img with 
    | [] -> []
    | r :: rs -> join r (transpose rs)

(* rest *)

type nonogram_spec = {rows: int list list; cols: int list list}

(* nie mam już pomysłu dlaczego to się może psuć *)
(* próbuje wszystkiego *)
let get_rid_of_zeros nono = 
    let rec groz lis = 
        match lis with 
        | [] -> []
        | 0 :: xs -> groz xs
        | x :: xs -> x :: (groz xs)
    in {rows = List.map groz nono.rows; cols = List.map groz nono.cols}

let solve_nonogram nono =
    let non = get_rid_of_zeros nono
    in build_candidate (non.rows) (List.length (non.cols))
    |> List.filter (fun xss -> transpose xss |> verify_rows non.cols)
