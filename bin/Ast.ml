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

let show_op = function
  | Plus -> "+"
  | Times -> "*"
  | Minus -> "-"
  | Div -> "/"
;;

let rec show_term t =
  match t with
  | Lit str -> "'" ^ str ^ "'"
  | App (t, s) -> show_term t ^ " " ^ show_term s
  | Lam (s, t) -> "(λ" ^ s ^ "." ^ show_term t ^ ")"
  | IntLit i -> string_of_int i
  | BinOp (t1, op, t2) -> " ( " ^ show_term t1 ^ " " ^ show_op op ^ " " ^ show_term t2 ^ " ) "
;;

module StringSet = Set.Make (String)

(*
   α-equivalence (not a good implementation)
*)
let rec alpha n = alpha_intern true StringSet.empty n

and alpha_intern bl bound_vars n = function
  | Lit s when StringSet.mem s bound_vars
      -> Lit n
  | Lam (arg, body) when not bl (* when the bool is false, we should not change argnames anymore *)
      -> Lam (arg, alpha_intern bl bound_vars n body)
  | Lam (arg, body) when arg != n && not (StringSet.mem arg bound_vars) 
      -> Lam (n, alpha_intern false (StringSet.add arg bound_vars) n body)
  | App (fn, arg) 
      -> App (alpha_intern bl bound_vars n fn, alpha_intern bl bound_vars n arg)
  | t -> t
  [@@ocamlformat "disable"]
