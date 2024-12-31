open Ast
module StringMap = Map.Make (String)

type env = term StringMap.t

open struct
  module Smap = StringMap
end

exception Reduce_error of string

let helper op a b =
  let fn = fn_of_op op in
  match a, b with
  | IntLit i0, IntLit i1 -> IntLit (fn i0 i1)
  (* the typechecker should make sure that this condition holds *)
  | a, b ->
    raise
    @@ Reduce_error
         ("Incombatible types for a, b (" ^ show_term a ^ ", " ^ show_term b ^ ")")
;;

let rec reduce env = function
  | Lit name -> reduce env (Smap.find name env)
  | (Bexp _ | IntLit _ | Lam _) as exp -> exp
  | App (t0, t1) ->
    let t1_re = reduce env t1 in
    (match t0 with
     | Lam (name, body) -> reduce (Smap.add name t1_re env) body
     | Lit name ->
       (* the typechecker should make sure this exists *)
       let lam = reduce env (Smap.find name env) in
       reduce env (App (lam, t1))
     | _ -> raise (Reduce_error "apply (e.g. 'f a') not a lambda or value"))
  | BinOp (t0, op, t1) -> helper op (reduce env t0) (reduce env t1)
  | Decl (name, t0) -> reduce (Smap.add name t0 env) t0
  | LetIn (name, t0, t1) ->
    let t0_re = reduce env t0 in
    reduce (Smap.add name t0_re env) t1
  | If (t0, t1, t2) ->
    (* the typechecker should make sure this conditions holds *)
    (match reduce env t0 with
     | Bexp true -> reduce env t1
     | Bexp false -> reduce env t2
     | _ -> raise (Reduce_error "if <cond> not of type bool"))
;;

let reduce_module decls =
  List.fold_left
    (fun acc v ->
       match v with
       | Decl (name, _) as term ->
         let res = reduce acc term in
         Smap.add name res acc
       | _term -> raise (Reduce_error "Illegal top level expression"))
    Smap.empty
    decls
;;
