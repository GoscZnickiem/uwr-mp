(* 1) build_row *)

let sum l = 
    let rec it acc l = 
        match l with
        | [] -> acc
        | x :: xs -> it (x + acc) xs
    in it 0 l

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

(* 2) build_candidate *)

let build_candidate spec size = 
    let rec f imgs spec = 
        match spec with 
        | [] -> imgs
        | x :: xs ->
            f (List.concat_map (fun img -> List.map (fun row -> img @ [row]) (build_row x size)) imgs) xs
    in f [[]] spec

(* 3) verify_row *)

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

(* 4) verify_rows *)

let verify_rows spec img =
    let rec f spec img = 
        match spec with 
        | [] -> true
        | x :: xs ->
            match img with 
            | [] -> true
            | row :: rest -> 
                if verify_row x row then f xs rest else false
    in f spec img

(* 5) transpose *)

let rec transpose img =
    match img with 
    | [] -> []
    | [] :: _ -> []
    | _ -> List.map List.hd img :: transpose (List.map List.tl img)

(* rest *)

let ( let* ) xs ys = List.concat_map ys xs

let rec choose m n =
  if m > n then [] else m :: choose (m+1) n

type nonogram_spec = {rows: int list list; cols: int list list}

let solve_nonogram nono =
    build_candidate (nono.rows) (List.length (nono.cols))
    |> List.filter (fun xss -> transpose xss |> verify_rows nono.cols)

(* testy *)

let example_1 = {
  rows = [[2];[1];[1]];
  cols = [[1;1];[2]]
}

let example_2 = {
  rows = [[2];[2;1];[1;1];[2]];
  cols = [[2];[2;1];[1;1];[2]]
}

let sasNono nono = 
    let viz list = List.iter (fun a -> (List.iter (fun b -> if b then print_string "□" else print_string "■") a; print_endline "")) list
    in let vizRes list = List.iter (fun a -> viz a; print_endline ""; print_endline "") list
    in vizRes (solve_nonogram nono);;

let print_row row =
    List.iter (fun pixel -> print_string (if pixel then "□" else "◼")) row;
    print_newline ()

let print_image image =
    List.iter (fun row -> print_row row) image;
    print_newline ()

let print_image_list image_list =
    List.iter (fun image -> print_image image) image_list;;

