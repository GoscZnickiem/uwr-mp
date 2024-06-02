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

(* CSE *)

module Stack = struct
    type 'a t = 'a list

    let empty = []

    let is_empty s =
        match s with
        | [] -> true
        | _ -> false

    let push x s = x :: s

    let pop s =
        match s with
        | [] -> None
        | _ :: xs -> Some xs

    let top s = 
        match s with
        | [] -> None
        | x :: _ -> Some x

    let index v s =
        let rec dist d v s = 
            match s with
            | [] -> None
            | x :: rest -> 
                if x = v then Some d
                else dist (d+1) v rest
        in dist 0 v s

    let rec gen_name e = 
        let name = "_var_" ^ string_of_int (Random.int 1000000)
        in if List.mem name e
            then gen_name e
        else name
end

let rec equiv e1 env1 e2 env2 = 
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
    | Var var1, Var var2 ->
        (match Stack.index (var1,true) env1, Stack.index (var2,true) env2 with
        | None, None -> var1 = var2
        | Some t1, Some t2 -> t1 = t2
        | _,_ -> false)
    | Let (var1, v1, e1), Let (var2, v2, e2) -> 
        equiv v1 env1 v2 env2 && 
        equiv e1 (Stack.push (var1,true) env1) e2 (Stack.push (var2,true) env2)
    | _,_ -> false

let rec vars e = 
    match e with
    | Binop (_, e1, e2) -> vars e1 @ vars e2
    | If (c, e1, e2) -> vars c @ vars e1 @ vars e2
    | Let (n, v, e) -> n :: (vars v @ vars e)
    | Var v -> [v]
    | _ -> []

let rec replace_in t tenv e env v = 
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
        (match replace_in t tenv e1 env v, replace_in t tenv e2 (Stack.push (n,false) env) v with
        | Some r, _ -> Some (Let (n, r, e2))
        | _, Some r -> Some (Let (n, e1, r))
        | None, None -> None)
    | _ -> None

let rec search e env t tenv v = 
    match e with
    | Int _ -> None
    | Bool _ -> None
    | Var _ -> None
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
                (match search e1 env t tenv v, search e2 (Stack.push (n,false) env) t tenv v with
                    | Some (r, x, e), _ -> Some (Let (n, r, e2), x, e)
                    | _, Some (r, x, e) -> Some (Let (n, e1, r), x, e)
                    | None, None -> None)
            | _ -> None

let rec cse e = 
    let name = Stack.gen_name ( vars e) in
    match e with
    | Binop (op, e1, e2) ->
        (match search e1 [] e2 [] name with
        | Some (r1, r2, e) -> Some (Let (name, e, Binop (op, r1, r2)))
        | None -> match cse e1, cse e2 with
            | Some r, _ -> Some (Binop (op, r, e2))
            | _, Some r -> Some (Binop (op, e1, r))
            | None, None -> None)
    | If (c, e1, e2) ->
        (match search c [] e1 [] name, search c [] e2 [] name, search e1 [] e2 [] name with
        | Some (r1, r2, e), _, _ -> Some (Let (name, e, If (r1, r2, e2)))
        | _, Some (r1, r2, e), _ -> Some (Let (name, e, If (r1, e1, r2)))
        | _, _, Some (r1, r2, e) -> Some (Let (name, e, If (c, r1, r2)))
        | None, None, None -> match cse c, cse e1, cse e2 with
            | Some r, _, _ -> Some (If (r, e1, e2))
            | _, Some r, _ -> Some (If (c, r, e2))
            | _, _, Some r -> Some (If (c, e1, r))
            | None, None, None -> None)
    | Let (n, e1, e2) ->
        (match search e1 [] e2 [(n,false)] name with
        | Some (r1, r2, e) -> Some (Let (name, e, Let (n, r1, r2)))
        | None -> match cse e1, cse e2 with
            | Some r, _ -> Some (Let (n, r, e2))
            | _, Some r -> Some (Let (n, e1, r))
            | None, None -> None)
    | _ -> None
