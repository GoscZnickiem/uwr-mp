open Ast

type frame = (var * int) list * int
let emptyframe = ([], 0)
type stack = frame list

let append (st : stack) (n : int) : stack = (fun (fr, s) -> (fr, s + n)) (List.hd st) :: (List.tl st)
let alloc (st : stack) (v : var) : stack = (fun (fr, s) -> ((v, s + 1) :: fr, s + 1)) (List.hd st) :: (List.tl st)

let get_var_addr (v : var) (st : stack) : cmd list = 
    let rec s_rec v st = 
        let (fr, s) = List.hd st and st = List.tl st 
        in match List.assoc_opt v fr with
        | Some i -> [LEA (s - i)]
        | None -> [LOAD (s - 1)] @ s_rec v st
    in [TOP] @ s_rec v st

let sepc = ':' let sep = String.make 1 sepc

let full_fun_name (f : name) (callst : name list) : name = String.concat sep (callst @ [f])

let get_fun (f : name) (callst : name list) (fs : name list) : name * int = 
    let rec names lst = 
        match lst with
        | _ :: tl -> full_fun_name f lst :: names tl
        | [] -> [f]
    in let res = List.find (fun n -> List.exists (fun x -> n = x) fs) (names callst)
    in res, List.length callst - List.length (String.split_on_char sepc res) + 1

let push_sframe (fspec : int) (st : stack) : cmd list = 
    let rec p_rec i st = 
        let (fr, s) = List.hd st and st = List.tl st 
        in if i = 0 then [LEA (s - List.length fr)] else [LOAD (s - 1)] @ p_rec (i - 1) st
    in [TOP] @ p_rec fspec st @ [PUSH]

let rec compile_aexp (e : aexp) (st : stack) (callst : name list) (fs : name list) : cmd list =
    match e with
    | Int n -> [CONST n]
    | Var v -> get_var_addr v st @ [LOAD 0]
    | Binop(op, e1, e2) -> compile_aexp e1 st callst fs @ [PUSH] @ compile_aexp e2 (append st 1) callst fs @ [PRIM op]
    | Call (f, args) -> 
        let rec push_args args st = 
            match args with
            | a :: ax -> compile_aexp a st callst fs @ [PUSH] @ push_args ax (append st 1)
            | [] -> []
        in let (fname, fspec) = get_fun f callst fs
        in push_sframe fspec st @ push_args args (append st 1) @ [CALL fname] @ [LEAVE (List.length args + 1)]

let rec compile_bexp (e : bexp) (st : stack) (callst : name list) (fs : name list) : cmd list =
    match e with
    | Bool b -> if b then [CONST 1] else [CONST 0]
    | Cmp(op, e1, e2) -> compile_aexp e1 st callst fs @ [PUSH] @ compile_aexp e2 (append st 1) callst fs @ [CMP op]
    | And(e1, e2) -> compile_bexp e1 st callst fs @ [BRANCH( compile_bexp e2 st callst fs, [CONST 0] )]
    | Or(e1, e2) -> compile_bexp e1 st callst fs @ [BRANCH( [CONST 1], compile_bexp e2 st callst fs )]
    | Not e -> compile_bexp e st callst fs @ [BRANCH( [CONST 0], [CONST 1] )]

let rec compile_stmt (s : stmt) (st : stack) (callst : name list) (fs : name list) : cmd list = 
    match s with
    | Block ss -> List.concat_map (fun s -> compile_stmt s st callst fs) ss
    | Assgn (v, e) -> get_var_addr v st @ [PUSH] @ compile_aexp e (append st 1) callst fs @ [STORE 0]
    | If (c, e1, e2) -> compile_bexp c st callst fs @ [BRANCH( compile_stmt e1 st callst fs, compile_stmt e2 st callst fs )]
    | While (c, e) -> [WHILE( compile_bexp c st callst fs, compile_stmt e st callst fs )]
    | Read v -> get_var_addr v st @ [PUSH; READ; STORE 0]
    | Write e -> compile_aexp e st callst fs @ [WRITE]
    | Return e -> compile_aexp e st callst fs

let compile_fun (name : name) (vars : var list) (code : stmt) (callst : name list) (fs : name list) (stack : stack) : name * cmd list =
    let varc = List.length vars
    in full_fun_name name callst, [ENTER varc] @ (compile_stmt code stack (name :: callst) fs) @ [LEAVE varc; RET]

let rec compile_funcs (funcs : func list) (callst : name list) (fs : name list) (stack : stack) : (name * cmd list) list =
    List.concat_map (fun (Func(name, args, vars, localf, code)) -> 
        let newstack = List.fold_left alloc (emptyframe :: stack) ([":stack#frame#addr:"] @ args @ [":ret#adrr:"] @ vars)
        in compile_fun name vars code callst fs newstack :: compile_funcs localf (name :: callst) fs newstack
    ) funcs

let rec gen_fun_names (funcs : func list) (prefix : string) : name list = 
    List.concat_map (fun (Func(name, _, _, fs, _)) -> (prefix ^ name) :: gen_fun_names fs (prefix ^ name ^ sep)) funcs

let compile_prog ((vars, funcs, main) : prog) : vm_prog = 
    let stack = List.fold_left alloc [emptyframe] vars
    and varc = List.length vars
    and fnames = gen_fun_names funcs ""
    in [ENTER varc] @ (compile_stmt main stack [] fnames) @ [LEAVE varc], compile_funcs funcs [] fnames stack
