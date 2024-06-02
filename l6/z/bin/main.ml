(* zad 3 *)

type 'a symbol =
    | T of string (* symbol terminalny *)
    | N of 'a (* symbol nieterminalny *)

type 'a grammar = ('a * ('a symbol list ) list ) list

let rec generate (grammar : 'a grammar) key = 
    let rec it expr = 
        match expr with
        | T str :: xs -> str :: (it xs)
        | N nt :: xs -> generate grammar nt :: (it xs)
        | [] -> [] 
    in let gens = List.assoc key grammar 
    in let gen = List.nth gens (Random.int (List.length gens))
    in String.concat "" (it gen)

let expr : unit grammar =
    [() , [[ N (); T "+"; N ()];
        [N (); T "*"; N ()];
        [T "("; N (); T ")" ];
        [T "1" ];
        [T "2" ]]]

let pol : string grammar =
    ["zdanie", [[N "grupa-podmiotu"; N "grupa-orzeczenia"]];
        "grupa-podmiotu", [[N "przydawka"; N "podmiot"]];
        "grupa-orzeczenia", [[N "orzeczenie"; N"dopelnienie"]]; 
        "przydawka", [[T "Piekny "];
            [T "Bogaty "];
            [T "Wesoly "]]; 
        "podmiot", [[T "policjant "];
            [T "student "];
            [T "piekarz "]];
        "orzeczenie", [[T "zjadl "];
            [T "pokochal "];
            [T "zobaczyl "]];
        "dopelnienie", [[T "zupe."];
            [T "studentke."];
            [T "sam siebie." ];
            [T "instytut informatyki." ]]
    ]

(* zad 4 *)

let parens_ok parens = 
    let rec it parens left right = 
        match parens with
        | [] -> left = right
        | '(' :: rest -> it rest (left+1) right
        | ')' :: rest -> if left = right then false else it rest left (right+1)
        | _ -> false
    in it (List.of_seq (String.to_seq parens)) 0 0

(* zad 5 *)

let parens_ok parens = 
    let flip p =
        match p with
        | ')' -> '('
        | ']' -> '['
        | '}' -> '{'
        | _ -> 'x'
    in let rec it parens stack = 
        match parens with 
        | [] -> stack = []
        | x :: xs ->
            match x with
            | '(' | '[' | '{' -> it xs (x :: stack)
            | _ -> let f = flip x 
                in if f = 'x' || stack = [] then false else
                if f = List.hd stack then it xs (List.tl stack) else false
    in it (List.of_seq (String.to_seq parens)) []


