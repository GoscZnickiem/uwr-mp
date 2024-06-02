open Ast

type stack = (var * int) list * int
let grow ((s, c) : stack) (n : int) : stack = (s, c + n)
let alloc (v : var) ((s, c) : stack) : stack = (v, c + 1) :: s, c + 1

let search_var (v : var) ((s, c) : stack) : int = c - List.assoc v s

let sep = "::"

let fun_full_name (f : name) (cst : name list) : name =
    if cst = [] then f else String.concat sep cst ^ sep ^ f

let search_fun (f : name) (cst : name list) (fs : name list) : name = 
    let rec names lst = 
        match lst with
        | _ :: tl -> fun_full_name f lst :: (names tl)
        | [] -> [f]
    in List.find (fun n -> List.exists (fun x -> n = x) fs) (names cst)

let rec compile_aexp (e : aexp) (st : stack) (cst : name list) (fs : name list) : cmd list =
    match e with
    | Int n -> [CONST n]
    | Var v -> [TOP; LOAD (search_var v st)]
    | Binop(op, e1, e2) -> compile_aexp e1 st cst fs @ [PUSH] @ compile_aexp e2 (grow st 1) cst fs @ [PRIM op]
    | Call (f, args) -> 
        let rec push_args args st = 
            match args with
            | a :: ax -> compile_aexp a st cst fs @ [PUSH] @ push_args ax (grow st 1)
            | [] -> []
        in (push_args args st) @ [CALL (search_fun f cst fs)] @ [LEAVE (List.length args)]

let rec compile_bexp (e : bexp) (st : stack) (cst : name list) (fs : name list) : cmd list =
    match e with
    | Bool b -> if b then [CONST 1] else [CONST 0]
    | Cmp(op, e1, e2) -> compile_aexp e1 st cst fs @ [PUSH] @ compile_aexp e2 (grow st 1) cst fs @ [CMP op]
    | And(e1, e2) -> compile_bexp e1 st cst fs @ [BRANCH( compile_bexp e2 st cst fs, [CONST 0] )]
    | Or(e1, e2) -> compile_bexp e1 st cst fs @ [BRANCH( [CONST 1], compile_bexp e2 st cst fs )]
    | Not e -> compile_bexp e st cst fs @ [BRANCH( [CONST 0], [CONST 1] )]

let rec compile_stmt (s : stmt) (st : stack) (cst : name list) (fs : name list) : cmd list = 
    match s with
    | Block ss -> List.concat_map (fun s -> compile_stmt s st cst fs) ss
    | Assgn (v, e) -> [TOP; PUSH] @ compile_aexp e (grow st 1) cst fs @ [STORE (search_var v st)] 
    | If (c, e1, e2) -> compile_bexp c st cst fs @ [BRANCH( compile_stmt e1 st cst fs, compile_stmt e2 st cst fs )]
    | While (c, e) -> [WHILE( compile_bexp c st cst fs, compile_stmt e st cst fs )]
    | Read v -> [TOP; PUSH; READ; STORE (search_var v st)]
    | Write e -> compile_aexp e st cst fs @ [WRITE]
    | Return e -> compile_aexp e st cst fs

type procedure = var list * var list * stmt

let rec gen_fun_names (fs : func list) (prefix : string) : name list = 
    match fs with
    | Func(name, _, _, fs, _) :: fss -> 
        (prefix ^ name) :: gen_fun_names fs (prefix ^ name ^ sep) @ gen_fun_names fss prefix
    | [] -> []

let compile_fun (name : name) (args : var list) (vars : var list) (code : stmt) (cst : name list) (fs : name list) : name * cmd list =
    let st_args = List.fold_left (fun s v -> alloc v s) ([], 0) args
    in let st_ret = grow st_args 1
    in let st = List.fold_left (fun s v -> alloc v s) st_ret vars
    in let varc = List.length vars
    in fun_full_name name cst, [ENTER varc] @ (compile_stmt code st (name :: cst) fs) @ [LEAVE varc; RET]

let rec compile_funcs (f : func list) (cst : name list) (fs : name list) : (name * cmd list) list =
    match f with
    | Func(name, args, vars, localf, code) :: fx -> 
        compile_fun name args vars code cst fs ::
        compile_funcs localf (name :: cst) fs @
        compile_funcs fx cst fs
    | [] -> []

let compile_prog ((vars, funcs, main) : prog) : vm_prog = 
    let stack = List.fold_left (fun s v -> alloc v s) ([], 0) vars
    in let varc = List.length vars
    and fnames = gen_fun_names funcs ""
    in [ENTER varc] @ (compile_stmt main stack [] fnames) @ [LEAVE varc], compile_funcs funcs [] fnames
