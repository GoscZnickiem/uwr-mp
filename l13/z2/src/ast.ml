(* abstract syntax tree *)

type bop = Mult | Div | Add | Sub | Eq | Lt | Gt | Leq | Geq | Neq | And | Or

type ident = string
type cname = string

type pattern =
  | PWildcard
  | PUnit
  | PVar  of ident
  | PInt  of int
  | PBool of bool
  | PCtor of cname * pattern list

type expr =
  | Unit
  | Int of int
  | Bool of bool
  | Var of ident
  | Binop of bop * expr * expr
  | If of expr * expr * expr
  | Let of ident * expr * expr
  | Fun of ident * expr
  | App of expr * expr
  | Fst  of expr
  | Snd  of expr
  | Ctor of cname * expr list
  | Match of expr * clause list
  | Raise
  | Try of expr * expr

and clause = pattern * expr
