type nonogram_spec = {rows: int list list; cols: int list list}

let sum list = List.fold_left (fun acc x -> acc + x+1) 0 list;;
(* funkcja zwracająca listę booli długości row_len -
   - na block_len pozycjach od index w górę true, na pozostałych false *)
let rec build_basic_row block_len row_len index =
   if row_len <= 0 then []
   else if block_len <= 0 then false::build_basic_row block_len (row_len-1) index
   else 
      if index = 0 then true::build_basic_row (block_len-1) (row_len-1) index
      else false::build_basic_row block_len (row_len-1) (index-1)

(* zwraca listę wszystkich mozliwych basic rows *)
let build_list_of_basic_rows block_len row_len = 
   let rec it i res = 
      if i<0 then res
      else 
      it (i-1) ((build_basic_row block_len row_len i)::res)
   in it (row_len-block_len) []

   
let join list list_of_lists = 
   let rec it res list_of_lists = 
   match list_of_lists with 
   |[]-> List.rev res
   |xs::ys -> it ((list@xs)::res) ys
   in it [] list_of_lists

let build_list_of_empty_lists count = 
   let rec it res i = 
      if i=count then res
      else it ([]::res) (i+1)
   in it [] 0

(* specyfikacja wiersza --> lista wierszy spełniających *)
let build_row ps n = 
   let rec build row_info row_len =
      match row_info with 
      | [] -> [build_basic_row 0 row_len 0]
      | [x] -> build_list_of_basic_rows x row_len 
      | x::xs -> 
         let sum_xs = (sum xs)-1 in 
         let rec it left_part_len right_part_len res = 
            if  right_part_len < sum_xs then res
            else let left_part_row = (build_basic_row 0 left_part_len 0)@(build_basic_row x x 0)@[false] in 
            it (left_part_len+1) (right_part_len-1) (join left_part_row (build xs right_part_len)@res)
         in it 0 (row_len-x-1) []
   in build ps n
   
let build_candidate pss n = 
   let rec build all_rows_info row_len = 
      match all_rows_info with 
      |[] -> [[]]
      |x::xs -> 
         let possible_rows_of_x = build_row x row_len in 
         let rec it rows_of_x res = 
            match rows_of_x with 
            |[] -> res
            |r::rest_of_rows -> it rest_of_rows ((join [r] (build xs row_len))@res)
         in it possible_rows_of_x []
      in build pss n

  
  
let verify_row ps xs =
   let rec verify_row_rec qs ys true_count =
      match qs, ys with
      |[],[] -> true
      |hd::[], [] -> true_count==hd
      |hd::tail, [] -> false
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



let rec verify_rows pss xss = 
   match pss, xss with 
   |[], [] -> true
   |p::ps, x::xs -> (verify_row p x) && (verify_rows ps xs)
   |[], _::_ -> false
   |_::_,[] -> false

let rec join_row list list_of_lists = 
   match list, list_of_lists with
   |[],[] -> []
   |[],_::_ -> list_of_lists
   |_::_ , [] -> [list]
   |x::xs, r::rs -> (x::r)::(join_row xs rs)

let rec list_to_list_of_lists list = 
   match list with 
   |[] -> []
   |x::xs -> [x]::(list_to_list_of_lists xs)
   
let rec transpose xss = 
   match xss with
   |[] -> []
   |[x] -> list_to_list_of_lists x
   |row::rest -> join_row row (transpose rest)

let solve_nonogram nono =
   build_candidate (nono.rows) (List.length (nono.cols)) |> List.filter (fun xss -> transpose xss |> verify_rows
   nono.cols)

(* time measurement *)
open Unix

let measure_time f x y =
    let start_time = gettimeofday () in
    let result = f x y in
    let end_time = gettimeofday () in
    let execution_time = end_time -. start_time in
    (result, execution_time)

let print_row row =
    List.iter (fun pixel -> print_string (if pixel then "□" else "◼")) row;
    print_newline ()

let print_image image =
    List.iter (fun row -> print_row row) image;
    print_newline ()

let print_image_list image_list =
    List.iter (fun image -> print_image image) image_list;;

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
    rows = [[3];[2;1];[3];[2];[4];[1;4];[6];[1];[2]];
    cols = [[1;2];[3;1];[1;5];[7;1];[5];[3]]
}

let example_4 = {
    rows = [[2;1];[1;3];[1;2];[3];[4];[1]];
    cols = [[1];[5];[2];[5];[2;1];[2]]
}

let example_5 = {
    rows = [[4];[6];[2;2];[2;2];[6];[4];[2];[2];[2]];
    cols = [[9];[9];[2;2];[2;2];[4];[4];[1]]
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
