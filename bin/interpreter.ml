open Ast
module StringMap = Map.Make (String)

type env = term StringMap.t

open struct
  module Smap = StringMap
end

exception Reduce_error of string

let fresh =
  let subs = ref Smap.empty in
  fun s ->
    let i = ref 0 in
    let s' = ref s in
    while Smap.mem !s' !subs do
      s' := s ^ string_of_int !i;
      incr i
    done;
    !s'
;;

(*
   Do not use _ to match other cases as we want to catch them if 
    we change the AST later on

    subee memonic for substitutee
*)
let rec substitute ~x:what ~(with_ : Ast.term) ~in_:haystack =
  let s = ("substitute " ^ what ^ " with " ^ Ast.show_term with_ ^ " in " ^ show_term haystack) in
  let sub = fun s -> substitute ~x:what ~with_ ~in_:s in
  let ret =
    match haystack with
    | Lit y when what = y -> with_
    | Lit _ as l -> l
    | Bexp _ as b -> b
    | IntLit _ as i -> i
    (*
       (λ x. λ y. y x) x

    Here the x would be shadowed right away.
    *)
    | Lam (param, _) as lam when param = what -> lam
    (*
       a global x
             v
     (λ y. y x (λ x. y x)) x
    *)
    | Lam (param, body) ->
      (* [ (λ x . (λ y . y x )) x ] *)
      (match with_ with
       | Lit with_y when with_y = param ->
         let new_param = fresh param in
         let new_body = substitute ~x:param ~with_:(Lit new_param) ~in_:body in
         Lam (new_param, sub new_body)
       | _ -> Lam (param, sub body))
    | App (left, right) -> App (sub left, sub right)
    | BinOp (left, op, right) -> BinOp (sub left, op, sub right)
    (* same as function *)
    | Decl (name, _) as decl when name = what -> decl
    | Decl (name, body) ->
      (match with_ with
       | Lit with_y when with_y = name ->
         let new_name = fresh name in
         let new_body = substitute ~x:name ~with_:(Lit new_name) ~in_:body in
         Decl (new_name, substitute ~x:what ~with_ ~in_:new_body)
       | _ -> Decl (name, sub body))
    | LetIn (name, body, expr) ->
      (match with_ with
       | Lit with_y when with_y = name ->
         let new_name = fresh name in
         (* let ... in ... is not recursive and its therefore its fine to only change 
          the name in the expr, not the body
         *)
         let new_expr = substitute ~x:name ~with_:(Lit new_name) ~in_:expr in
         LetIn (new_name, sub body, sub new_expr)
       | _ -> LetIn (name, sub body, sub expr))
    | If (bexp, then_, else_) -> If (sub bexp, sub then_, sub else_)
  in
  print_endline (s ^ " now: " ^ Ast.show_term ret);
  ret
;;

let helper op a b =
  let fn = fn_of_op op in
  match a, b with
  | IntLit i0, IntLit i1 -> IntLit (fn i0 i1)
  (* the typechecker should make sure that this condition holds *)
  | a, b ->
    raise
    @@ Reduce_error
         ("Incombatible types for a, b\n\t" ^ show_term a ^ "\n\t" ^ show_term b ^ ")")
;;

(*
   let foo = \x  \x . x

    foo<1><"string"> ->> "string"
*)

(* TODO should we create a new env and only keep top-level information ? *)
let rec reduce env fn x =
  let s = ref ("reducing " ^ Ast.show_term x) in
  (match x with
   | Lit y -> s := !s ^ " with " ^ Ast.show_term (Smap.find y env)
   | IntLit _ | Bexp _ | Lam _ -> s := !s ^ " ×"
   | _ -> ());
  fn !s;
  match x with
  | Lit name -> reduce env fn (Smap.find name env)
  | (Bexp _ | IntLit _ | Lam _) as exp -> exp
  | App (t0, t1) ->
    let t0_re = reduce env fn t0 in
    let t1_re = reduce env fn t1 in
    (match t0_re with
     | Lam (name, body) ->
       let sub = substitute ~x:name ~with_:t1_re ~in_:body in
       reduce env fn sub
     | Lit name ->
       let lam = Smap.find name env in
       reduce env fn (App (lam, t1_re))
     | truth ->
       raise
         (Reduce_error
            ("apply (e.g. 'f a') not a lambda or value is: "
             ^ show_term_constructor truth
             ^ " "
             ^ show_term truth)))
  | BinOp (t0, op, t1) -> helper op (reduce env fn t0) (reduce env fn t1)
  | Decl (name, t0) -> reduce (Smap.add name t0 env) fn t0
  | LetIn (name, t0, t1) ->
    let t0_re = reduce env fn t0 in
    let subst = substitute ~x:name ~with_:t0_re ~in_:t1 in
    reduce env fn subst
    (* reduce (Smap.add name t0_re env) fn t1 *)
  | If (t0, t1, t2) ->
    (* the typechecker should make sure this conditions holds *)
    (match reduce env fn t0 with
     | Bexp true -> reduce env fn t1
     | Bexp false -> reduce env fn t2
     | _ -> raise (Reduce_error "if <cond> not of type bool"))
;;

let reduce_module decls =
  List.fold_left
    (fun acc v ->
       match v with
       | Decl (name, _rhs) as term  ->
         let res = reduce acc print_endline term in
         Smap.add name res acc
       | _term -> raise (Reduce_error "Illegal top level expression"))
    Smap.empty
    decls
;;
