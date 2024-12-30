type term =
  | Lit of string
  | Bexp of bool
  | IntLit of int
  | Lam of string * term
  | App of term * term
  | BinOp of term * op * term
  | Decl of string * term
  | LetIn of string * term * term
  | If of term * term * term

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

let fn_of_op = function
  | Plus -> ( + )
  | Times -> ( * )
  | Minus -> ( - )
  | Div -> ( / )
;;

let rec show_term t =
  match t with
  | Lit str            -> str
  | App (t, s)         -> "(" ^ show_term t ^ "<" ^ show_term s ^ ">)"
  | Lam (s, t)         -> "(λ" ^ s ^ "." ^ show_term t ^ ")"
  | IntLit i           -> string_of_int i
  | BinOp (t1, op, t2) -> "( " ^ show_term t1 ^ " " ^ show_op op ^ " " ^ show_term t2 ^ " )"
  | Decl (name, t)     -> "let " ^ name ^ " = " ^ show_term t
  | LetIn (name, t, e) -> "[let " ^ name ^ " = " ^ show_term t ^ " in " ^ show_term e ^ "]"
  | If (bexp, t, f)    -> "if " ^ show_term bexp ^ " then " ^ show_term t ^ " else " ^ show_term f
  | Bexp bl            -> string_of_bool bl
 [@@ocamlformat "disable"]
