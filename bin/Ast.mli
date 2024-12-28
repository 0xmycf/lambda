type term =
  | Lit of string
  | Lam of string * term
  | App of term * term
  | IntLit of int
  | BinOp of term * op * term

and op =
  | Plus
  | Minus
  | Times
  | Div

val show_term : term -> string

(*
   α-equivalence
*)
val alpha : string -> term -> term
