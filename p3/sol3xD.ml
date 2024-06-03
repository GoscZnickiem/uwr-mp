open Ast (* Kod nie zawiera komentarzy aby poprawić czytelność *)
type frm = (var * int) list * int
type stck = frm list
let appnd (st : stck) (n : int) : stck = (fun (fr, s) -> (fr, s + n)) (List.hd st) :: (List.tl st)
let allc (st : stck) (v : var) : stck = (fun (fr, s) -> ((v, s + 1) :: fr, s + 1)) (List.hd st) :: (List.tl st)
let gvd (v : var) (st : stck) : cmd list = let rec s_rec v st = let (fr, s) = List.hd st and st = List.tl st in match List.assoc_opt v fr with | Some i -> [LEA (s - i)] | None -> [LOAD (s - 1)] @ s_rec v st in [TOP] @ s_rec v st
let spc = ':'
let sp = String.make 1 spc
let ffn (f : name) (ast : name list) : name = String.concat sp (ast @ [f])
let gf (f : name) (hr : name list) (fs : name list) : name * int = let rec nms lst = match lst with | _ :: tl -> ffn f lst :: nms tl | [] -> [f] in let res = List.find (fun n -> List.exists (fun x -> n = x) fs) (nms hr) in res, List.length hr - List.length (String.split_on_char spc res) + 1
let psf (fspc : int) (st : stck) : cmd list = let rec p_rec i st = let (fr, s) = List.hd st and st = List.tl st in if i = 0 then [LEA (s - List.length fr)] else [LOAD (s - 1)] @ p_rec (i - 1) st in [TOP] @ p_rec fspc st @ [PUSH]
let rec cmax (e : aexp) (st : stck) (hr : name list) (fs : name list) : cmd list = match e with | Int n -> [CONST n] | Var v -> gvd v st @ [LOAD 0] | Binop(op, e1, e2) -> cmax e1 st hr fs @ [PUSH] @ cmax e2 (appnd st 1) hr fs @ [PRIM op] | Call (f, args) -> let rec psa args st = match args with | a :: ax -> cmax a st hr fs @ [PUSH] @ psa ax (appnd st 1) | [] -> [] in let (fnm, fspc) = gf f hr fs in psf fspc st @ psa args (appnd st 1) @ [CALL fnm] @ [LEAVE (List.length args + 1)]
let rec cmbx (e : bexp) (st : stck) (hr : name list) (fs : name list) : cmd list = match e with | Bool b -> if b then [CONST 1] else [CONST 0] | Cmp(op, e1, e2) -> cmax e1 st hr fs @ [PUSH] @ cmax e2 (appnd st 1) hr fs @ [CMP op] | And(e1, e2) -> cmbx e1 st hr fs @ [BRANCH( cmbx e2 st hr fs, [CONST 0] )] | Or(e1, e2) -> cmbx e1 st hr fs @ [BRANCH( [CONST 1], cmbx e2 st hr fs )] | Not e -> cmbx e st hr fs @ [BRANCH( [CONST 0], [CONST 1] )]
let rec cmst (s : stmt) (st : stck) (vrc : int) (hr : name list) (fs : name list) : cmd list = match s with | Block ss -> List.concat_map (fun s -> cmst s st vrc hr fs) ss | Assgn (v, e) -> gvd v st @ [PUSH] @ cmax e (appnd st 1) hr fs @ [STORE 0] | If (c, e1, e2) -> cmbx c st hr fs @ [BRANCH( cmst e1 st vrc hr fs, cmst e2 st vrc hr fs )] | While (c, e) -> [WHILE( cmbx c st hr fs, cmst e st vrc hr fs )] | Read v -> gvd v st @ [PUSH; READ; STORE 0] | Write e -> cmax e st hr fs @ [WRITE] | Return e -> cmax e st hr fs @ [LEAVE vrc; RET]
let cmf (name : name) (vrc : int) (cd : stmt) (ast : name list) (fs : name list) (stck : stck) : name * cmd list = ffn name ast, [ENTER vrc] @ cmst cd stck vrc (name :: ast) fs
let rec cmpf (fncs : func list) (ast : name list) (fs : name list) (stck : stck) : (name * cmd list) list = List.concat_map (fun (Func(name, args, vrs, localf, cd)) -> let nwst = List.fold_left allc (([], 0) :: stck) ([":frm#addr:"] @ args @ [":ret#adrr:"] @ vrs) in cmf name (List.length vrs) cd ast fs nwst :: cmpf localf (name :: ast) fs nwst) fncs
let rec gfn (fncs : func list) (prfx : string) : name list = List.concat_map (fun (Func(name, _, _, fs, _)) -> (prfx ^ name) :: gfn fs (prfx ^ name ^ sp)) fncs
let compile_prog ((vrs, fncs, cd) : prog) : vm_prog = let stck = List.fold_left allc [([], 0)] vrs and vrc = List.length vrs and fnms = gfn fncs "" in [ENTER vrc] @ cmst cd stck 0 [] fnms @ [LEAVE vrc], cmpf fncs [] fnms stck
