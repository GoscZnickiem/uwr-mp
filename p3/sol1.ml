open Ast

type stack = (var * int) list * int
let grow ((s, c) : stack) (n : int) : stack = (s, c + n)
let alloc (v : var) ((s, c) : stack) : stack = (v, c + 1) :: s, c + 1

let search_var (v : var) ((s, c) : stack) : int = c - List.assoc v s

let rec compile_aexp (e : aexp) (st : stack) : cmd list =
    match e with
    | Int n -> [CONST n]
    | Var v -> [TOP; LOAD (search_var v st)]
    | Binop(op, e1, e2) -> compile_aexp e1 st @ [PUSH] @ compile_aexp e2 (grow st 1) @ [PRIM op]
    | Call (f, args) -> 
        let rec push_args args st = 
            match args with
            | a :: ax -> compile_aexp a st @ [PUSH] @ push_args ax (grow st 1)
            | [] -> []
        in (push_args args st) @ [CALL f] @ [LEAVE (List.length args)]

let rec compile_bexp (e : bexp) (st : stack) : cmd list =
    match e with
    | Bool b -> if b then [CONST 1] else [CONST 0]
    | Cmp(op, e1, e2) -> compile_aexp e1 st @ [PUSH] @ compile_aexp e2 (grow st 1) @ [CMP op]
    | And(e1, e2) -> compile_bexp e1 st @ [BRANCH( compile_bexp e2 st, [CONST 0] )]
    | Or(e1, e2) -> compile_bexp e1 st @ [BRANCH( [CONST 1], compile_bexp e2 st )]
    | Not e -> compile_bexp e st @ [BRANCH( [CONST 0], [CONST 1] )]

let rec compile_stmt (s : stmt) (st : stack) : cmd list = 
    match s with
    | Block ss -> List.concat_map (fun s -> compile_stmt s st) ss
    | Assgn (v, e) -> [TOP; PUSH] @ compile_aexp e (grow st 1) @ [STORE (search_var v st)] 
    | If (c, e1, e2) -> compile_bexp c st @ [BRANCH( compile_stmt e1 st, compile_stmt e2 st )]
    | While (c, e) -> [WHILE( compile_bexp c st, compile_stmt e st )]
    | Read v -> [TOP; PUSH; READ; STORE (search_var v st)]
    | Write e -> compile_aexp e st @ [WRITE]
    | Return e -> compile_aexp e st

type procedure = var list * var list * stmt

let compile_procedure ((args, vars, proc) : procedure) : cmd list =
    let st_args = List.fold_left (fun s v -> alloc v s) ([], 0) args
    in let st_ret = grow st_args 1
    in let st = List.fold_left (fun s v -> alloc v s) st_ret vars
    in let var_count = List.length vars
    in let all = [ENTER var_count] and deall = [LEAVE var_count]
    in all @ (compile_stmt proc st) @ deall

let compile_func (Func(name, args, vars, _, s) : func) : name * cmd list =
    (name, compile_procedure (args, vars, s) @ [RET])

let compile_prog ((vars, funcs, main) : prog) : vm_prog = 
    let st = List.fold_left (fun s v -> alloc v s) ([], 0) vars
    in let var_count = List.length vars
    in let all = [ENTER var_count] and deall = [LEAVE var_count]
    in (all @ (compile_stmt main st) @ deall), List.map compile_func funcs
