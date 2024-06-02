let ( let* ) xs ys = List.concat_map ys xs

let sum l = List.fold_left (+) 0 l

let rec append_with v num xs = 
    if num = 0 then xs else
    append_with v (num - 1) (v :: xs)

let build_row spec size = 
    let blacks = sum spec in let whites = size - blacks in let groups = List.length spec in 
    let rec create l spec groups whites can_black = 
        let w = if whites >= groups && whites != 0 then 
            create (false :: l) spec groups (whites - 1) true 
            else []
        and b = if can_black && groups != 0 then 
            create (append_with true (List.hd spec) l) (List.tl spec) (groups - 1) whites false 
            else []
        in if w = [] && b = [] then [l] else w @ b
    in create [] (List.rev spec) groups whites true

let build_candidate spec size = 
    let rec f imgs spec = 
        match spec with 
        | [] -> imgs
        | x :: xs ->
            let rows = build_row x size in
            f (let* img = imgs in List.map (fun row -> row :: img) rows) xs
    in f [[]] (List.rev spec)

let rec check_blacks row num = 
    if num = 0 then (true, row) else
        match row with 
        | [] -> false, []
        | true :: rest -> check_blacks rest (num - 1)
        | false :: _ -> false, []

let rec verify_row spec row = 
    if row = [] then true else
        match spec with 
        | [] -> if List.hd row then false else verify_row [] (List.tl row)
        | x :: xs -> 
            if List.hd row then 
                let rest = check_blacks row x in 
                if fst rest then verify_row xs (snd rest) else false 
            else verify_row spec (List.tl row)

let rec verify_rows spec img =
    match spec with 
    | [] -> true
    | x :: xs ->
        if verify_row x (List.hd img) then verify_rows xs (List.tl img) else false

let rec transpose img =
    match img with 
    | [] -> []
    | [] :: _ -> []
    | _ -> List.map List.hd img :: transpose (List.map List.tl img)

type nonogram_spec = {rows: int list list; cols: int list list}

let solve_nonogram nono =
    build_candidate (nono.rows) (List.length (nono.cols))
    |> List.filter (fun xss -> transpose xss |> verify_rows nono.cols)
