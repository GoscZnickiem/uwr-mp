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

let rec check_blacks row num = 
    if num = 0 then (true, row) else
        match row with 
        | [] -> false, []
        | true :: rest -> check_blacks rest (num - 1)
        | false :: _ -> false, []

let rec verify_row spec row = 
    if row = [] then spec = [] else
        match spec with 
        | [] -> if List.hd row then false else verify_row [] (List.tl row)
        | x :: xs -> 
            if List.hd row then 
                let cb = check_blacks row x in 
                if fst cb then verify_row xs (snd cb) else false 
            else verify_row spec (List.tl row)

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

let solve_nonogram nono =
    build_candidate (nono.rows) (List.length (nono.cols))
    |> List.filter (fun xss -> transpose xss |> verify_rows nono.cols)
