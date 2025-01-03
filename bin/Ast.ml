type op =
  | Plus
  | Minus
  | Times
  | Div

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

let rec show_term_constructor = function
  | Lit _ -> "Lit"
  | Bexp _ -> "Bexp"
  | IntLit _ -> "IntLit"
  | Lam _ -> "Lam"
  | App _ -> "App"
  | BinOp _ -> "BinOp"
  | If _ -> "If"
  | LetIn _ -> "LetIn"
  | Decl _ -> "Decl"
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

let rec pretty_print_ast_internal depth t = 
  let pref = "\n" ^ ts depth in 
  let pref' = "\n" ^ ts (depth + 1) in 
  let next = fun x -> "\n" ^ ts (depth + 1) ^ pretty_print_ast_internal (depth + 1) x in
  match t with 
  | Lit str            -> str
  | App (t, s)         -> "(" ^ next t ^ "<" ^ next s ^ pref' ^ ">" ^ pref ^ ")" ^ pref
  | Lam (s, t)         -> "(λ" ^ s ^ "." ^ next t ^ pref ^ ")" ^ pref
  | IntLit i           -> string_of_int i
  | BinOp (t1, op, t2) -> "( " ^ next t1 ^ " " ^ show_op op ^ " " ^ next t2 ^ pref ^ " )" ^ pref
  | Decl (name, t)     -> "let " ^ name ^ " = " ^ next t
  | LetIn (name, t, e) -> "[let " ^ name ^ " = " ^ next t ^ " in " ^ next e ^ pref ^ "]" ^ pref
  | If (bexp, t, f)    -> "if " ^ next bexp ^ " then " ^ next t ^ " else " ^ next f
  | Bexp bl            -> string_of_bool bl
 [@@ocamlformat "disable"]

and ts depth = String.make depth '\t'

let pretty_print_ast = pretty_print_ast_internal 0

module IR : sig

  module SMap : Map.S with type key = string

  val substitute : thing:string -> _in:term -> _with:string -> term
  val uniqulify_module : string SMap.t ->  term list -> term list

end = struct
  let count = ref 0

  let fresh base =
    incr count;
    base ^ string_of_int !count
  ;;

  (*
     dont do catch all pattern there, we wont get a warning if we change
     the AST later on otherwise
  *)
  let rec substitute ~thing ~_in ~_with =
    let sub = fun _in -> substitute ~thing ~_in ~_with in
    match _in with
    | Lit name when name = thing -> Lit _with
    | Lit _ as l -> l
    | Bexp _ as b -> b
    | IntLit _ as i -> i
    | App (t0, t1) -> App (sub t0, sub t1)
    | BinOp (t0, p, t1) -> BinOp (sub t0, p, sub t1)
    | Decl (s, t0) ->
      (* attention! we have to keep the term unique now *)
      let new_name = fresh s in
      let new_t0 = substitute ~thing:s ~_in:t0 ~_with:new_name in
      Decl (new_name, sub new_t0)
    | LetIn (s, t, t2) ->
      let new_name = fresh s in
      (* attention! we have to keep the term unique now *)
      let new_t2 = substitute ~thing:s ~_in:t2 ~_with:new_name in
      LetIn (new_name, sub t, sub new_t2)
    | If (if_, then_, else_) -> If (sub if_, sub then_, sub else_)
    | Lam (name, body) ->
      let new_name = fresh name in
      let new_body = substitute ~thing:name ~_with:new_name ~_in:body in
      Lam (new_name, sub new_body)
  ;;

  module SMap = Map.Make (String)

  type subt = string SMap.t

  let rec uniqulify_module sub = function
    | [] -> []
    | hd :: tl -> failwith "wip" :: uniqulify_module sub tl
  ;;
end
