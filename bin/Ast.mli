type term =
  | Lit of string
  | Bexp of bool
  | IntLit of int
  | Lam of string * term
  | App of term * term
  | BinOp of term * op * term
  | Decl of string * term
  (* | LetIn of string * term * term *)
  | If of term * term * term

and op =
  | Plus
  | Minus
  | Times
  | Div

val show_term : term -> string
val fn_of_op : op -> int -> int -> int
