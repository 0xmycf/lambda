module Smap = struct
  include Map.Make (String)

  let find_def key def map =
    match find_opt key map with
    | None -> def
    | Some x -> x
  ;;

  let string_values f s = List.map (fun (_, b) -> b) @@ to_list (mapi f s)
  let right_union a b = union (fun _ _ a -> Some a) a b
  let left_union a b = union (fun _ a _ -> Some a) a b
end

module Sset = Set.Make (String)

type typecheck_error =
  | Undefined_variable of string
  | Type_mismatch of string * string
  | Infinite_type of string

exception HM_exn of typecheck_error

let string_of_tc_error e =
  let open Printf in
  "error: "
  ^
  match e with
  | Type_mismatch (_a, _b) ->
    sprintf "Type mismtach between \n(got)\t%s\n(need)\t%s\n" _a _b
  | Infinite_type _a -> sprintf "Cannot construct infinite type %s" _a
  | Undefined_variable _a -> sprintf "%s is not defined" _a
;;

module Type = struct
  type t =
    | TyVar of string
    | TyCon of string * t list

  let rec show = function
    | TyVar name -> "TyVar(" ^ name ^ ")"
    | TyCon ("lambda", [ a; b ]) -> "(" ^ pretty_show a ^ " -> " ^ pretty_show b ^ ")"
    | TyCon (name, rest) ->
      let rest_string = String.concat ", " @@ List.map (fun x -> show x) rest in
      "TyCon(" ^ name ^ ", [" ^ rest_string ^ "])"

  and pretty_show = function
    | TyVar name -> name
    | TyCon ("lambda", _) as l -> show l
    | TyCon (name, []) -> name
    | TyCon (name, rest) ->
      let rest_string = String.concat ", " @@ List.map (fun x -> pretty_show x) rest in
      name ^ "[" ^ rest_string ^ "]"
  ;;

  let tyvar x = TyVar ("t" ^ x)

  let rec free = function
    | TyVar tv -> Sset.singleton tv
    | TyCon (_, rest) ->
      List.fold_left (fun acc v -> Sset.union (free v) acc) Sset.empty rest
  ;;

  type typ = t

  let bool_type = TyCon ("bool", [])
  let int_type = TyCon ("int", [])
  let string_type = TyCon ("string", [])
  let lambda_type a b = TyCon ("lambda", [ a; b ])
  let bin_op_type a b c = lambda_type a (lambda_type b c)
  (**
    Something like '1 + 3'
    here (+) :: int -> int -> int
   *)
  let arith_bin_op_type = lambda_type int_type (lambda_type int_type int_type) 

  let rec apply sub t =
    match t with
    | TyVar ty -> Smap.find_def ty t sub
    | TyCon (name, rest) -> TyCon (name, List.map (apply sub) rest)
  ;;

  module Substitution = struct
    type t = typ Smap.t

    module M = Smap

    let show sub = Show.show show Smap.string_values sub

    (* Left merge *)
    let merge l r = Smap.union (fun _ a _ -> Some a) l r

    let compose_all ls =
      List.fold_right (fun acc v -> merge (Smap.map (apply acc) v) acc) ls Smap.empty
    ;;
  end

  let rec unify : t -> t -> Substitution.t =
    fun a b ->
    match a, b with
    | a, b when a = b -> Substitution.M.empty
    | TyVar a, con -> bind a con
    | con, TyVar b -> bind b con
    | (TyCon (n1, _) as a), (TyCon (n2, _) as b) when n1 != n2 ->
      raise (HM_exn (Type_mismatch (show b, show a)))
    | TyCon (_, typs1), TyCon (_, typs2) ->
      let open Substitution in
      List.fold_left2
        (fun acc l r -> M.right_union (unify (apply acc l) (apply acc r)) acc)
        M.empty
        typs1
        typs2

  and bind tv typ =
    match Sset.find_opt tv (free typ) with
    | None -> Substitution.M.singleton tv typ
    | Some _ -> raise (HM_exn (Infinite_type (show typ)))
  ;;
end

module TypeScheme = struct
  type t = Sset.t * Type.t

  let show scheme =
    let values = Sset.fold (fun key acc -> acc ^ key ^ "\n") scheme "" in
    "{ " ^ values ^ " }"
  ;;

  module M = Sset

  let show (set, t) =
    let as_list = Sset.to_list set in
    let forall = String.concat " " as_list in
    let prefix =
      "∀ "
      ^ (match forall with
         | "" -> "∅"
         | s -> s)
      ^ ". "
    in
    prefix ^ Type.show t
  ;;

  let poly : 'a -> t = fun x -> Sset.empty, x

  (* typevars and type *)
  let apply sub (tvs, ty) =
    let open Type in
    let tvs' = Sset.filter (fun k -> not (Smap.mem k sub)) tvs in
    tvs', Type.apply sub ty
  ;;

  let instantiate (bounds, t) freshs =
    let sub = Smap.of_list @@ List.combine (Sset.to_list bounds) freshs in
    Type.apply sub t
  ;;

  let compare (_, a) (_, b) = compare a b
  let len (vars, _) = Sset.cardinal vars

  module Ops = struct
    let ( -/ ) a b = M.diff a b
  end
end

module Environment = struct
  include Smap

  type t = TypeScheme.t Smap.t

  let show sub = Show.show TypeScheme.show Smap.string_values sub
  let apply sub t = Smap.map (fun x -> TypeScheme.apply sub x) t

  module Ops = struct
    let ( +: ) env (var, ty) = add var ty env
    let ( +:: ) env (var, ty) = add var (TypeScheme.poly ty) env
  end
end

module type Algorithm = sig
  type input
  type output

  val infer : Ast.t -> input -> output

  val infer_many
    :  Ast.t list
    -> Environment.t (* TODO we must see if we can generalize this *)
end

module type W_type = sig
    include Algorithm

    val gen : Environment.t -> Type.t -> TypeScheme.t
  end
  with type input = Environment.t
  with type output = Type.Substitution.t * Type.t

module W : W_type = struct
  type input = Environment.t
  type output = Type.Substitution.t * Type.t

  let c = ref 0

  let fresh () =
    let now = !c in
    incr c;
    Type.tyvar (string_of_int now)
  ;;

  let new_beta poly =
    let amnt = TypeScheme.len poly in
    List.init amnt (fun _ -> fresh ())
  ;;

  (* Environment.t -> Type.t -> TypeScheme.t *)
  let gen env tau : TypeScheme.t =
    let tau_vars = Type.free tau in
    TypeScheme.M.filter (fun x -> not (Environment.mem x env)) tau_vars, tau
  ;;

  let rec infer t env =
    let open Ast in
    let module Ty = Type in
    let module Sub = Type.Substitution in
    let module Env = Environment in
    let module Poly = TypeScheme in
    let open Env.Ops in
    let id = Smap.empty in
    match t with
    (* as afar as I can tell this is CON *)
    | Bexp _ -> id, Ty.bool_type
    | IntLit _ -> id, Ty.int_type
    (* VAR *)
    | Lit x ->
      let tau =
        try Env.find x env with
        | Not_found -> raise (HM_exn (Undefined_variable x))
      in
      id, Poly.instantiate tau (new_beta tau)
    (* FN *)
    | Lam (x, e) ->
      let beta = fresh () in
      let s1, t1 = infer e (env +:: (x, beta)) in
      let s1_beta = Ty.apply s1 beta in
      s1, Ty.lambda_type s1_beta t1
    (* APP *)
    | App (e1, e2) ->
      let s1, t1 = infer e1 env in
      let s2, t2 = infer e2 (Env.apply s1 env) in
      let beta = fresh () in
      let s3 = Ty.unify (Ty.apply s2 t1) (Ty.lambda_type t2 beta) in
      Sub.compose_all [ s3; s2; s1 ], Ty.apply s3 beta
    (* LET *)
    | LetIn (x, e1, e2) ->
      let s1, t1 = infer e1 env in
      let s2, t2 = infer e2 (Env.apply s1 env +: (x, gen env t1)) in
      Sub.compose_all [ s2; s1 ], t2
    (* only functions are recursive *)
    | Decl (f, e) ->
      (match e with
       | Lam (_, _) as l ->
         let beta = fresh () in
         let s1, t1 = infer l (env +:: (f, beta)) in
         let s2 = Ty.unify beta t1 in
         let s3 = Sub.compose_all [ s2; s1 ] in
         s3, Ty.apply s3 beta
       | otherwise ->
         let s1, t1 = infer otherwise env in
         s1, t1)
    (* We only have arithmetic (int's) binops at the moment *)
    | BinOp (left, _op, right) ->
        let s1, left_t = infer left env in
        let s2, right_t = infer right (Env.apply s1 env) in
        let beta = fresh () in
        let s3 = Ty.(unify arith_bin_op_type (bin_op_type left_t right_t beta)) in
        Sub.compose_all [ s3; s2; s1 ], Ty.apply s3 beta
    (* Could be modeled through combinators ! TODO make the parser modular to switch between different implementations *)
    | If (if_, then_, else_) ->
      let s1, if_t = infer if_ env in
      let s4 = Ty.unify Ty.bool_type if_t in
      let env' = Env.apply s4 env in (* this makes sure that the if_ variable is recognised as such *)
      let s2, then_t = infer then_ env' in
      let s3, else_t = infer else_ env' in
      let s5 = Ty.unify (Ty.apply s4 then_t) (Ty.apply s4 else_t) in
      let sub = Sub.compose_all [ s5; s4; s3; s2; s1 ] in
       sub, Ty.apply sub else_t
  ;;

  let infer_many terms =
    let open Ast in
    let go env' v =
      match v with
      | Decl (name, expr) as decl ->
        (match infer decl env' with
         | _, ty -> Environment.add name (gen env' ty) env'
         | exception HM_exn exn ->
           raise
           @@ Failure (string_of_tc_error exn ^ " in " ^ name ^ " = " ^ Ast.show_term expr))
      | _expr -> failwith "Invalid top level expression"
    in
    List.fold_left go Environment.empty terms
  ;;
end
