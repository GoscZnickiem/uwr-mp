(* Maciej Zgierski *)
(* nr indeksu: 345278 *)
(* Zadanie na pracownie nr 2 *)

(* abstract syntax tree *)

type bop = Mult | Div | Add | Sub | Eq | Lt | Gt | Leq | Geq | Neq

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | If of expr * expr * expr
  | Var of ident
  | Let of ident * expr * expr

(* W mojej implementacji środowisko jest stosem zmiennych wiązanych przez wyrażenia Let. Dodatkowo 
   do każdego wiązania zapamiętuje informację o tym, czy wiązanie nastąpiło wewnątrz podwyrażenia
   e1 lub e2 (wartość true) czy też poza podwyrażeniem ale wewnątrz całego e (wartość false).
   Zmienne związane "po drodze" między korzeniem e a podwyrażeniem e1/e2 nazywam "półzwiązanymi".
   Taki zabieg pozwala sprawnie przetestować równoważność podwyrażeń e1 i e2 wyrażenia e - dwie zmienne 
   w e1 i e2 są równoważne jeśli ich wiązanie nastąpiło tak samo daleko (mają równy index wiązania), ale 
   napotkanie na jakąkolwiek zmienną półzwiązaną automatycznie dyskwalifikuje e1 i e2 z bycia równoważnymi.
   
   Mając prosty sposób na sprawdzanie równoważności reszta jest koncepcyjnie prosta: dla danego e szukam
   w jednym z jego podwyrażeń czegoś, co da się podstawić w drugim podwyrażeniu. Jeśli znajdę - podstawiam
   i kończę algorytm, a jeśli nie - każde z podwyrażeń e poddaje temu samemu algorytmowi. Wybierając podwyrażenia
   e1 i e2 należy jedynie pamiętać, aby wszelkie zmienne wiązane po drodze zmienne oznaczać jako półzwiązane. *)

type env = (ident * bool) list

(* Funkcja zwracająca indeks zmiennej związanej oraz informację o tym, czy jest związana czy półzwiązana *)
let index (v : ident) (s : env) =
    let rec dist d v s = 
        match s with
        | [] -> None
        | (x, b) :: rest -> 
            if x = v then Some (d, b)
            else dist (d+1) v rest
    in dist 0 v s

(* Funkcja sprawdzająca, czy dwa wyrażenia w swoich środowiskach są równoważne *)
let rec equiv (e1 : expr) (env1 : env) (e2 : expr) (env2 : env) = 
    match e1, e2 with
    | Int v1, Int v2 -> v1 = v2
    | Bool v1, Bool v2 -> v1 = v2
    | Binop (op1, l1, r1), Binop (op2, l2, r2) -> 
        op1 = op2 &&
        equiv l1 env1 l2 env2 &&
        equiv r1 env1 r2 env2
    | If(c1, l1, r1), If(c2, l2, r2) ->
        equiv c1 env1 c2 env2 &&
        equiv l1 env1 l2 env2 &&
        equiv r1 env1 r2 env2
    | Let (var1, v1, e1), Let (var2, v2, e2) -> 
        equiv v1 env1 v2 env2 && 
        equiv e1 ((var1,true) :: env1) e2 ((var2,true) :: env2)
    | Var var1, Var var2 ->
        (match index var1 env1, index var2 env2 with
        | Some (t1, true), Some (t2, true) -> t1 = t2
        | None, None -> var1 = var2
        | _,_ -> false)
    | _,_ -> false

(* Funkcja przeszukująca podwyrażenia e w poszukiwaniu wyrażenia t. Zwraca e z podstawioną
   zmienną v za jedno wystąpienie t lub None jeśli nic nie udało się znaleźć *)
let rec replace_in (t : expr) (tenv : env) (e : expr) (env : env) (v : ident) = 
    if equiv e env t tenv then Some (Var v)
    else
    match e with
    | Binop (op, e1, e2) ->
        (match replace_in t tenv e1 env v, replace_in t tenv e2 env v with
        | Some r, _ -> Some (Binop (op, r, e2))
        | _, Some r -> Some (Binop (op, e1, r))
        | None, None -> None)
    | If (c, e1, e2) ->
        (match replace_in t tenv c env v, replace_in t tenv e1 env v, replace_in t tenv e2 env v with
        | Some r, _, _ -> Some (If (r, e1, e2))
        | _, Some r, _ -> Some (If (c, r, e2))
        | _, _, Some r -> Some (If (c, e1, r))
        | None, None, None -> None)
    | Let (n, e1, e2) ->
        (match replace_in t tenv e1 env v, replace_in t tenv e2 ((n,false) :: env) v with
        | Some r, _ -> Some (Let (n, r, e2))
        | _, Some r -> Some (Let (n, e1, r))
        | None, None -> None)
    | _ -> None

(* Funkcja szukająca w podwyrażeniach e wyrażenia które da się podstawić w t przy pomocy replace_in. 
   Wyrażenie to jest więc wyrażeniem wspólnym dla e i t. Zwraca trójkę: 
   e z podstawionym wyrażeniem wspólnym, t z podstawionym wyrażeniem wspólnym, wyrażenie wspólne które zostało podstawione *)
let rec search (e : expr) (env : env) (t : expr) (tenv : env) (v : ident) = 
    match e with
    | Int _ | Bool _ | Var _ -> None
    | _ -> 
        match replace_in e env t tenv v with
        | Some x -> Some (Var v, x, e)
        | None ->
            match e with
            | Binop (op, e1, e2) ->
                (match search e1 env t tenv v, search e2 env t tenv v with
                    | Some (r, x, e), _ -> Some (Binop (op, r, e2), x, e)
                    | _, Some (r, x, e) -> Some (Binop (op, e1, r), x, e)
                    | None, None -> None)
            | If (c, e1, e2) ->
                (match search c env t tenv v ,search e1 env t tenv v, search e2 env t tenv v with
                    | Some (r, x, e), _, _ -> Some (If (r, e1, e2), x, e)
                    | _, Some (r, x, e), _ -> Some (If (c, r, e2), x, e)
                    | _, _, Some (r, x, e) -> Some (If (c, e1, r), x, e)
                    | None, None, None -> None)
            | Let (n, e1, e2) ->
                (match search e1 env t tenv v, search e2 ((n,false) :: env) t tenv v with
                    | Some (r, x, e), _ -> Some (Let (n, r, e2), x, e)
                    | _, Some (r, x, e) -> Some (Let (n, e1, r), x, e)
                    | None, None -> None)
            | _ -> None

(* Funkcja generująca nazwę zmiennej która jeszcze nie pojawiła się w e *)
let gen_name (e : expr) = 
    let rec vars e = 
        match e with
        | Binop (_, e1, e2) -> vars e1 @ vars e2
        | If (c, e1, e2) -> vars c @ vars e1 @ vars e2
        | Let (n, v, e) -> n :: (vars v @ vars e)
        | Var v -> [v]
        | _ -> []
    in let rec gen vars = 
        let name = "_var_" ^ string_of_int (Random.int 1000000)
        in if List.mem name vars
            then gen vars
        else name
    in gen (vars e)

(* CSE *)
let rec cse (e : expr) = 
    let name = gen_name e in
    match e with
    | Binop (op, e1, e2) ->
        (match search e1 [] e2 [] name with
        | Some (r1, r2, e) -> Some (Let (name, e, Binop (op, r1, r2)))
        | None -> 
            match cse e1, cse e2 with
            | Some r, _  -> Some (Binop (op, r, e2))
            | _, Some r  -> Some (Binop (op, e1, r))
            | None, None -> None)
    | If (c, e1, e2) ->
        (match search c [] e1 [] name, search c [] e2 [] name, search e1 [] e2 [] name with
        | Some (r1, r2, e), _, _ -> Some (Let (name, e, If (r1, r2, e2)))
        | _, Some (r1, r2, e), _ -> Some (Let (name, e, If (r1, e1, r2)))
        | _, _, Some (r1, r2, e) -> Some (Let (name, e, If (c, r1, r2)))
        | None, None, None -> 
            match cse c, cse e1, cse e2 with
            | Some r, _, _ -> Some (If (r, e1, e2))
            | _, Some r, _ -> Some (If (c, r, e2))
            | _, _, Some r -> Some (If (c, e1, r))
            | None, None, None -> None)
    | Let (n, e1, e2) ->
        (match search e1 [] e2 [(n,false)] name with
        | Some (r1, r2, e) -> Some (Let (name, e, Let (n, r1, r2)))
        | None -> 
            match cse e1, cse e2 with
            | Some r, _  -> Some (Let (n, r, e2))
            | _, Some r  -> Some (Let (n, e1, r))
            | None, None -> None)
    | _ -> None
