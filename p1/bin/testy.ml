=====================
tu wklej swój kod cały
=====================

let ( let* ) xs ys = List.concat_map ys xs

(* 1) build_row *)

let msum l = List.fold_left (+) 0 l

let rec mappend_with v num xs = 
    if num = 0 then xs else
    mappend_with v (num - 1) (v :: xs)

let mbuild_row spec size = 
    let whites = size - msum spec and groups = List.length spec in 
    if whites < (groups - 1) then [] else
    let rec create r spec groups whites last_white = 
        let w = if whites >= groups && whites != 0 then 
            create (false :: r) spec groups (whites - 1) true 
            else []
        and b = if last_white && groups != 0 then 
            create (mappend_with true (List.hd spec) r) (List.tl spec) (groups - 1) whites false 
            else []
        in if w = [] && b = [] then [r] else w @ b
    in create [] (List.rev spec) groups whites true

(* 2) build_candidate *)

let mbuild_candidate spec size = 
    let rec f imgs spec = 
        match spec with 
        | [] -> imgs
        | x :: xs -> 
            let rows = mbuild_row x size in
            f (let* img = imgs in List.map (fun row -> row :: img) rows) xs
    in f [[]] (List.rev spec)

(* 3) verify_row *)

let mverify_row spec row = 
    let rec rc row = 
        match row with 
        | [] -> ([], 0)
        | true :: rs -> let recr = rc rs in (fst recr, snd recr + 1)
        | false :: rs -> let recr = rc rs in if snd recr <> 0 then ((snd recr)::(fst recr), 0) else recr
    in let pair = rc row
    in let recreated = if snd pair <> 0 then (snd pair)::(fst pair) else fst pair
    in recreated = spec

(* 4) verify_rows *)

let rec mverify_rows spec img =
    match spec with 
    | [] -> true
    | x :: xs ->
        if mverify_row x (List.hd img) then mverify_rows xs (List.tl img) else false

(* 5) transpose *)

let rec mtranspose img =
    let rec join row ls =
        match row, ls with 
        | [], _ -> ls
        | _, [] -> List.map (fun x -> [x]) row
        | x :: xs, r :: rs -> (x::r) :: (join xs rs)
    in match img with 
    | [] -> []
    | r :: rs -> join r (mtranspose rs)

(* rest *)

type nonogram_spec = {rows: int list list; cols: int list list}

let mget_rid_of_zeros nono = 
    let rec groz lis = 
        match lis with 
        | [] -> []
        | 0 :: xs -> groz xs
        | x :: xs -> x :: (groz xs)
    in {rows = List.map groz nono.rows; cols = List.map groz nono.cols}

let msolve_nonogram nono =
    let non = mget_rid_of_zeros nono
    in mbuild_candidate (non.rows) (List.length (non.cols))
    |> List.filter (fun xss -> mtranspose xss |> mverify_rows non.cols)

(* ============================================================================ *)
(* ============================================================================ *)
(* ============================================================================ *)

let hsum list = List.fold_left (fun acc x -> acc + x+1) 0 list;;
(* funkcja zwracająca listę booli długości row_len -
   - na block_len pozycjach od index w górę true, na pozostałych false *)
let rec hbuild_basic_row block_len row_len index =
   if row_len <= 0 then []
   else if block_len <= 0 then false::hbuild_basic_row block_len (row_len-1) index
   else 
      if index = 0 then true::hbuild_basic_row (block_len-1) (row_len-1) index
      else false::hbuild_basic_row block_len (row_len-1) (index-1)

(* zwraca listę wszystkich mozliwych basic rows *)
let hbuild_list_of_basic_rows block_len row_len = 
   let rec it i res = 
      if i<0 then res
      else 
      it (i-1) ((hbuild_basic_row block_len row_len i)::res)
   in it (row_len-block_len) []

   
let hjoin list list_of_lists = 
   let rec it res list_of_lists = 
   match list_of_lists with 
   |[]-> List.rev res
   |xs::ys -> it ((list@xs)::res) ys
   in it [] list_of_lists

let hbuild_list_of_empty_lists count = 
   let rec it res i = 
      if i=count then res
      else it ([]::res) (i+1)
   in it [] 0

(* specyfikacja wiersza --> lista wierszy spełniających *)
let hbuild_row ps n = 
   let rec build row_info row_len =
      match row_info with 
      | [] -> [hbuild_basic_row 0 row_len 0]
      | [x] -> hbuild_list_of_basic_rows x row_len 
      | x::xs -> 
         let sum_xs = (hsum xs)-1 in 
         let rec it left_part_len right_part_len res = 
            if  right_part_len < sum_xs then res
            else let left_part_row = (hbuild_basic_row 0 left_part_len 0)@(hbuild_basic_row x x 0)@[false] in 
            it (left_part_len+1) (right_part_len-1) (hjoin left_part_row (build xs right_part_len)@res)
         in it 0 (row_len-x-1) []
   in build ps n
   
let hbuild_candidate pss n = 
   let rec build all_rows_info row_len = 
      match all_rows_info with 
      |[] -> [[]]
      |x::xs -> 
         let possible_rows_of_x = hbuild_row x row_len in 
         let rec it rows_of_x res = 
            match rows_of_x with 
            |[] -> res
            |r::rest_of_rows -> it rest_of_rows ((hjoin [r] (build xs row_len))@res)
         in it possible_rows_of_x []
      in build pss n
  
let hverify_row ps xs =
   let rec verify_row_rec qs ys true_count =
      match qs, ys with
      |[],[] -> true
      |hd::[], [] -> true_count==hd
      |_::_, [] -> false
      |[],_::_ -> 
         let rec verify row = 
            match row with 
            |[] -> true
            |z::zs -> z=false && verify zs 
         in verify ys
      |hd::tail, r::rs -> 
         if r then (
            if (true_count+1) > hd then 
               false 
            else 
               verify_row_rec (hd::tail) rs (true_count+1) ) 
         else (
            if true_count<>0 then 
               (if true_count<>hd then false else verify_row_rec tail rs 0)
            else verify_row_rec (hd::tail) rs 0 
         )
   in verify_row_rec ps xs 0

let rec hverify_rows pss xss = 
   match pss, xss with 
   |[], [] -> true
   |p::ps, x::xs -> (hverify_row p x) && (hverify_rows ps xs)
   |[], _::_ -> false
   |_::_,[] -> false

let rec hjoin_row list list_of_lists = 
   match list, list_of_lists with
   |[],[] -> []
   |[],_::_ -> list_of_lists
   |_::_ , [] -> [list]
   |x::xs, r::rs -> (x::r)::(hjoin_row xs rs)

let rec hlist_to_list_of_lists list = 
   match list with 
   |[] -> []
   |x::xs -> [x]::(hlist_to_list_of_lists xs)
   
let rec htranspose xss = 
   match xss with
   |[] -> []
   |[x] -> hlist_to_list_of_lists x
   |row::rest -> hjoin_row row (htranspose rest)

let hsolve_nonogram nono =
   hbuild_candidate (nono.rows) (List.length (nono.cols)) |> List.filter (fun xss -> htranspose xss |> hverify_rows
   nono.cols)

(* ============================================================================ *)
(* ============================================================================ *)
(* ============================================================================ *)

let print_row row =
    List.iter (fun pixel -> print_string (if pixel then "□" else "◼")) row;
    print_newline ()

let print_image image =
    List.iter (fun row -> print_row row) image;
    print_newline ()

let print_image_list image_list =
    List.iter (fun image -> print_image image) image_list;;

(* Funkcja generująca losową specyfikację nonogramu *)
let generate_nonogram_spec rows cols valid_spec =
  let rec generate_row_spec valid_spec =
    if valid_spec then
      List.init (Random.int cols) (fun _ -> Random.int (cols + 1))
    else
      List.init (Random.int (cols + 5) + cols + 1) (fun _ -> Random.int (cols + 5))
  in
  let rec generate_spec valid_spec num =
    if num = 0 then []
    else generate_row_spec valid_spec :: generate_spec valid_spec (num - 1)
  in
  { rows = generate_spec valid_spec rows; cols = generate_spec valid_spec cols }

(* Funkcja porównująca wyniki działania dwóch programów rozwiązujących nonogramy *)
let compare_solutions my_solution other_solution1 other_solution2 =
  my_solution = other_solution1 || my_solution = other_solution2

let print_spec spec = 
    let rec p l =
        match l with 
        | [] -> print_newline()
        | x :: xs -> 
            let rec pl l = 
                match l with 
                | [] -> ()
                | x :: xs -> print_int x; print_string ";" ;pl xs
            in print_string "["; pl x; print_string "]"; p xs
    in p spec.rows; p spec.cols

(* Wyniki działania dwóch programów rozwiązujących nonogramy *)
let check spec = 
    print_spec spec;
    let my_sol = solve_nonogram spec in
    print_endline "Twój tod działa";
    let msol = msolve_nonogram spec in
    print_endline "Kod Maćka działa";
    let hsol = hsolve_nonogram spec in
    print_endline "Kod Heleny działa";
    let passed = compare_solutions my_sol msol hsol in
    print_endline "Test comparison:";
    print_endline (if passed then "Passed" else "Failed");
    passed

let get_rid_of_zeros nono = 
    let rec groz lis = 
        match lis with 
        | [] -> []
        | 0 :: xs -> groz xs
        | x :: xs -> x :: (groz xs)
    in {rows = List.map groz nono.rows; cols = List.map groz nono.cols}

let rec test n x y =
    if n = 0 then () else
        let spec = get_rid_of_zeros (generate_nonogram_spec x y (Random.bool()))
        in if check spec then (print_newline(); test (n-1) x y) else ()

(* ============================================================================ *)
(* ============================================================================ *)
(* ============================================================================ *)

(* time measurement *)
open Unix

let measure_time f x y =
    let start_time = gettimeofday () in
    let result = f x y in
    let end_time = gettimeofday () in
    let execution_time = end_time -. start_time in
    (result, execution_time)

let solve_nonogram nono =
    let (build_res, build_time) = measure_time build_candidate (nono.rows) (List.length (nono.cols)) in
    let (sol_res, sol_time) = measure_time List.filter (fun xss -> transpose xss |> verify_rows nono.cols) build_res in
    Printf.printf "Build time: %f\nVerify time: %f\nBuild size: %d\n" build_time sol_time (List.length build_res);
    (* print_image_list build_res; *)
    sol_res

(* tests *)

let example_1 = {
  rows = [[2];[1];[1]];
  cols = [[1;1];[2]]
}

let example_2 = {
  rows = [[2];[2;1];[1;1];[2]];
  cols = [[2];[2;1];[1;1];[2]]
}

let example_3 = {
    rows = [[3];[2;1];[3;2];[2;2];[6];[1;5];[6];[1];[2]];
    cols = [[1;2];[3;1];[1;5];[7;1];[5];[3];[4];[3]]
}

let example_4 = {
    rows = [[2;1];[1;3];[1;2];[3];[4];[1]];
    cols = [[1];[5];[2];[5];[2;1];[2]]
}

let example_5 = {
    rows = [[4];[6];[2;2];[2;2];[6];[4];[2];[2];[2]];
    cols = [[9];[9];[2;2];[2;2];[4];[4];[]]
}

let example_6 = {
    rows = [[];[2];[1];[]];
    cols = [[];[2];[1];[]]
}

let big_example = {
  rows = [[1;2];[2];[1];[1];[2];[2;4];[2;6];[8];[1;1];[2;2]];
  cols = [[2];[3];[1];[2;1];[5];[4];[1;4;1];[1;5];[2;2];[2;1]]
}

let sasNono nono = 
    let viz list = List.iter (fun a -> (List.iter (fun b -> if b then print_string "□" else print_string "■") a; print_endline "")) list
    in let vizRes list = List.iter (fun a -> viz a; print_endline ""; print_endline "") list
    in vizRes (solve_nonogram nono);;


