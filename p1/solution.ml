let ( let* ) xs ys = List.concat_map ys xs

let rec choose m n =
  if m > n then [] else m :: choose (m+1) n

(* 1) build_row *)

let sum l = List.fold_left (+) 0 l

let rec append_with v num xs = 
    if num = 0 then xs else
    append_with v (num - 1) (v :: xs)

let build_row ps n = 
    let whites = n - sum ps and groups = List.length ps in 
    if whites < (groups - 1) then [] else
    let rec create r ps groups whites last_white = 
        let w = if whites >= groups && whites != 0 then 
            create (false :: r) ps groups (whites - 1) true 
            else []
        and b = if last_white && groups != 0 then 
            create (append_with true (List.hd ps) r) (List.tl ps) (groups - 1) whites false 
            else []
        in if w = [] && b = [] then [r] else w @ b
    in create [] (List.rev ps) groups whites true

(* 2) build_candidate *)

let build_candidate pss n = 
    let rec f imgs pss = 
        match pss with 
        | [] -> imgs
        | x :: xs -> 
            let rows = build_row x n in
            f (let* img = imgs in List.map (fun row -> row :: img) rows) xs
    in f [[]] (List.rev pss)

(* 3) verify_row *)

let verify_row ps xs = 
    let rec rc xs = 
        match xs with 
        | [] -> ([], 0)
        | true :: rs -> let recr = rc rs in (fst recr, snd recr + 1)
        | false :: rs -> let recr = rc rs in if snd recr <> 0 then ((snd recr)::(fst recr), 0) else recr
    in let pair = rc xs
    in let recreated = if snd pair <> 0 then (snd pair)::(fst pair) else fst pair
    in recreated = ps

(* 4) verify_rows *)

let rec verify_rows pss xss =
    match pss with 
    | [] -> true
    | p :: ps ->
        if verify_row p (List.hd xss) then verify_rows ps (List.tl xss) else false

(* 5) transpose *)

let rec transpose xss =
    let rec join row ls =
        match row, ls with 
        | [], _ -> ls
        | _, [] -> List.map (fun x -> [x]) row
        | x :: xs, r :: rs -> (x::r) :: (join xs rs)
    in match xss with 
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
