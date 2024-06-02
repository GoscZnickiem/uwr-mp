let sum l = 
    let rec it acc l = 
        match l with
        | [] -> acc
        | x :: xs -> it (x + acc) xs
    in it 0 l

let rec append xs ys = 
    match xs with
    | [] -> ys
    | x :: xs' -> x :: append xs' ys

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
        in if w = [] && b = [] then [l] else append w b
    in create [] (List.rev spec) groups whites true

let build_candidate spec size = 
    let rec f imgs spec = 
        match spec with 
        | [] -> imgs
        | x :: xs ->
        f (List.concat_map (fun img -> List.map (fun row -> row :: img) (build_row x size)) imgs) xs
    in f [[]] spec

let rec check_blacks row num = 
    if num = 0 then (true, row) else
        match row with 
        | [] -> false, []
        | true :: rest -> check_blacks rest (num - 1)
        | false :: _ -> false, []

let verify_row spec row = 
    let rec f spec row = 
        if row = [] then true else
            match spec with 
            | [] -> if List.hd row then false else f [] (List.tl row)
            | x :: xs -> 
                if List.hd row then 
                    let rest = check_blacks row x in 
                    if fst rest then f xs (snd rest) else false 
                else f spec (List.tl row)
    in f spec row

let verify_rows spec img =
    let rec f spec img = 
        match spec with 
        | [] -> true
        | x :: xs ->
            match img with 
            | [] -> true
            | row :: rest -> 
                if verify_row x row then f xs rest else false
    in f (List.rev spec) img

let transpose img =
    let rec it acc img = 
        match img with 
        | [] -> List.rev acc
        | [] :: _ -> List.rev acc
        | rows ->
            let col = List.map List.hd rows in 
            it (col :: acc) (List.map List.tl rows)
    in it [] img;;

(* let ( let* ) xs ys = List.concat_map ys xs *)
(**)
(* let rec choose m n = *)
(*   if m > n then [] else m :: choose (m+1) n *)

type nonogram_spec = {rows: int list list; cols: int list list}

let solve_nonogram nono =
    build_candidate (nono.rows) (List.length (nono.cols))
    |> List.filter (fun xss -> transpose xss |> verify_rows nono.cols)

let example_1 = {
  rows = [[2];[1];[1]];
  cols = [[1;1];[2]]
}

let example_2 = {
  rows = [[2];[2;1];[1;1];[2]];
  cols = [[2];[2;1];[1;1];[2]]
}

let big_example = {
  rows = [[1;2];[2];[1];[1];[2];[2;4];[2;6];[8];[1;1];[2;2]];
  cols = [[2];[3];[1];[2;1];[5];[4];[1;4;1];[1;5];[2;2];[2;1]]
}

let sasNono nono = 
    let viz list = List.iter (fun a -> (List.iter (fun b -> if b then print_string "■" else print_string "□") a; print_endline "")) list
    in let vizRes list = List.iter (fun a -> viz a; print_endline ""; print_endline "") list
    in vizRes (solve_nonogram nono);;

sasNono example_1;;
print_newline;;
print_newline;;
print_newline;;
sasNono example_2;;
print_newline;;
print_newline;;
print_newline;;
sasNono big_example;;
