type ident = string

type qbf =
    | Top (* ⊤ *)
    | Bot (* ⊥ *)
    | Var of ident (* x *)
    | Disj of qbf * qbf (* ∨ *)
    | Conj of qbf * qbf (* ∧ *)
    | Not of qbf (* ¬ *)
    | Forall of ident * qbf (* ∀ *)
    | Exists of ident * qbf (* ∃ *)

let rec subst (x : ident ) (s : qbf ) (f : qbf ) : qbf =
    match f with
    | Var v -> if v = x then s else Var v
    | Disj (a, b) -> Disj (subst x s a, subst x s b)
    | Conj (a, b) -> Conj (subst x s a, subst x s b)
    | Not a -> Not (subst x s a)
    | Forall (v, a) -> Forall (v, subst x s a)
    | Exists (v, a) -> Exists (v, subst x s a)
    | _ -> f

let rec eval (f : qbf ) : bool =
    match f with 
    | Top -> true
    | Bot -> false
    | Disj (a, b) -> eval a || eval b
    | Conj (a, b) -> eval a && eval b
    | Not a -> not (eval a)
    | Forall (v, a) -> eval (subst v Top a) && eval (subst v Bot a)
    | Exists (v, a) -> eval (subst v Top a) || eval (subst v Bot a)
    | Var v -> failwith ("Unbound value" ^ v)
