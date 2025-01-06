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
    (* Substitution.M.singleton tv typ *)
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
    | Lam (x, e) ->
      let beta = fresh () in
      let s1, t1 = infer e (env +:: (x, beta)) in
      let s1_beta = Ty.apply s1 beta in
      s1, Ty.lambda_type s1_beta t1
    | App (e1, e2) ->
      let s1, t1 = infer e1 env in
      let s2, t2 = infer e2 (Env.apply s1 env) in
      let beta = fresh () in
      let s3 = Ty.unify (Ty.apply s2 t1) (Ty.lambda_type t2 beta) in
      Sub.compose_all [ s3; s2; s1 ], Ty.apply s3 beta
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
    (* We only have arithmetic binops at the momment *)
    | BinOp (lhs, _, rhs) -> failwith "wip"
    | If (_, _, _) -> failwith "wip"
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

(*********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************
*********************************************************************************)

module LType = struct
  open Ppx_compare_lib.Builtin

  type ltypevar = { name : string } [@@deriving compare]

  (* something like this ...*)
  type ltype =
    (*
       - int
      - bool
      - List Int
      - etc.
    *)
    | Typ of string * ltype list
    (*
       - Abstract type vars such as t0, a, ...
    *)
    | TypVar of ltypevar
  [@@deriving compare]

  (*
     predefined types
  *)

  let type_bool = Typ ("bool", [])
  let type_int = Typ ("int", [])
  let type_string = Typ ("string", [])
  let lambda a b = Typ ("lambda", [ a; b ])
  let arith_op = lambda type_int (lambda type_int type_int)

  let string_of_ltypevar = function
    | { name } -> name
  ;;

  let rec string_of_ltype = function
    | Typ (s, []) -> s
    | Typ ("lambda", [ a; b ]) ->
      "(" ^ string_of_ltype a ^ " -> " ^ string_of_ltype b ^ ")"
    | Typ (s, ts) ->
      let sfied = List.map string_of_ltype ts in
      s ^ "<" ^ String.concat ", " sfied ^ ">"
    | TypVar v -> string_of_ltypevar v
  ;;
end

module CompareTypeVar = struct
  type t = LType.ltypevar

  let compare = LType.compare_ltypevar
end

open struct
  module TypeSet = Set.Make (CompareTypeVar)
end

(*
   Returns the free variables in t
*)
let rec tvs t =
  let open LType in
  let module TS = TypeSet in
  match t with
  | Typ (_, rest) -> List.fold_left (fun acc v -> TS.union (tvs v) acc) TS.empty rest
  | TypVar tv -> TS.singleton tv
;;

module TC_types = struct
  open LType

  (* also called -Scheme- or -forall- 
    ∀ x . x -> x
  *)
  type polytype = TypeSet.t * ltype

  let string_of_polytype ((s, l) : polytype) : string =
    let as_list = TypeSet.to_list s in
    let forall = String.concat " " @@ List.map string_of_ltypevar as_list in
    let prefix =
      match forall with
      | "" -> ""
      | s -> "∀ " ^ s ^ ". "
    in
    prefix ^ string_of_ltype l
  ;;

  module SMap = Map.Make (String)
  module TypeMap = Map.Make (CompareTypeVar)

  type context = polytype SMap.t
  type constraint_ = ltype * ltype
  type constraints = constraint_ list
  type subst = ltype TypeMap.t

  let tvs_polytype ((bound, expr) : polytype) =
    let ( /- ) = TypeSet.diff in
    tvs expr /- bound
  ;;

  let tvs_context (ctx : context) =
    let module TS = TypeSet in
    let values = Seq.map (fun (_, b) -> b) (SMap.to_seq ctx) in
    Seq.fold_left (fun acc v -> TS.union (tvs_polytype v) acc) TS.empty values
  ;;

  let rec apply : subst -> ltype -> ltype =
    fun s t ->
    match t with
    | Typ (name, rest) -> Typ (name, List.map (apply s) rest)
    | TypVar tv ->
      (match TypeMap.find_opt tv s with
       | None -> t
       | Some var -> var)
  ;;

  open struct
    let rec zip as' bs' =
      match as', bs' with
      | [], _ -> []
      | _, [] -> []
      | hd0 :: tl0, hd1 :: tl1 -> (hd0, hd1) :: zip tl0 tl1
    ;;

    let apply_constraint : subst -> constraint_ -> constraint_ =
      fun s (c1, c2) -> apply s c1, apply s c2
    ;;

    let compose : subst -> subst -> subst =
      fun s0 s1 ->
      let ( +/ ) = TypeMap.union (fun _ a _ -> Some a) in
      TypeMap.map (apply s0) (s0 +/ s1)
    ;;
  end

  (* inital var_id and inital constraints *)
  class infer ?(ii = 0) ?(cc = []) (ctx : context) =
    object (self)
      val mutable i = ii
      val mutable c : constraints = cc
      val context = ctx
      method constraint_ a b = c <- (a, b) :: c

      (* similar to local in the Reader Monad *)
      method copy_with fn = new infer ~ii:i ~cc:c (fn context)

      method private run_with_copy fn arg =
        let copy = self#copy_with fn in
        let t0 = copy#infer_private arg in
        self#update copy;
        t0

      method get_i_c = i, c

      method update (other : infer) =
        let ni, nc = other#get_i_c in
        i <- ni;
        c <- nc

      method fresh =
        let x = i in
        i <- i + 1;
        TypVar { name = "t" ^ string_of_int x }

      method generalize : ltype -> polytype =
        fun typ ->
          let free_t = tvs typ in
          let free_ctx = tvs_context ctx in
          let ( /- ) = TypeSet.diff in
          free_t /- free_ctx, typ

      method instantiate ((bound, t) : polytype) : ltype =
        let as_seq = TypeSet.to_seq bound in
        let vars = Seq.map (fun _ -> self#fresh) as_seq in
        let subst = TypeMap.of_list (zip (List.of_seq as_seq) (List.of_seq vars)) in
        apply subst t

      method infer ast : ltype * constraints =
        let t = self#infer_private ast in
        t, c

      (*
         Can throw Undefined_variable exn
      *)
      method infer_private (ast : Ast.term) : ltype =
        let open Ast in
        match ast with
        | Lit name ->
          (match SMap.find_opt name ctx with
           | None -> raise (HM_exn (Undefined_variable name))
           | Some x -> self#instantiate x)
        | Bexp _ -> type_bool
        | IntLit _ -> type_int
        | Lam (str, t) ->
          let arg = self#fresh in
          let ps = TypeSet.empty, arg in
          let body = self#run_with_copy (SMap.add str ps) t in
          Typ ("lambda", [ arg; body ])
        | App (t0, t1) as app ->
          let applicant = self#infer_private t0 in
          let arg = self#infer_private t1 in
          let ret = self#fresh in
          print_endline
          @@ Ast.show_term app
          ^ string_of_ltype applicant
          ^ " should be "
          ^ string_of_ltype (LType.lambda arg ret);
          self#constraint_ (LType.lambda arg ret) applicant;
          ret
        | BinOp (t0, _, t1) ->
          (* op is currently not relevant *)
          let first = self#infer_private t0 in
          let second = self#infer_private t1 in
          let ret = self#fresh in
          self#constraint_ LType.arith_op (lambda first (lambda second ret));
          ret
        | Decl (str, t0) ->
          (* generalize -after- infer_private *)
          let name = self#fresh in
          let ps = TypeSet.empty, name in
          let decl_body = self#run_with_copy (SMap.add str ps) t0 in
          self#constraint_ name decl_body;
          decl_body
        | LetIn (str, t0, t1) as term ->
          print_endline "term is: -----------------";
          print_endline @@ show_term term;
          (SMap.iter (fun key value ->
             print_endline @@ key ^ " : " ^ string_of_polytype value))
            context;
          print_endline @@ "checking rhs: " ^ show_term t0;
          let rhs = self#infer_private t0 in
          print_endline @@ "rhs: " ^ string_of_ltype rhs;
          let gen = self#generalize rhs in
          print_endline
          @@ "rhs generalized: "
          ^ Ast.show_term t0
          ^ ": "
          ^ string_of_polytype gen;
          let expr_body = self#run_with_copy (SMap.add str gen) t1 in
          print_endline
          @@ "expr_body "
          ^ Ast.show_term t1
          ^ ": "
          ^ string_of_ltype expr_body;
          print_endline "-- CONSTRAINTS ---------------\n";
          (List.iter (fun (l, r) ->
             print_endline @@ string_of_ltype l ^ " <-> " ^ string_of_ltype r))
            c;
          print_endline "\n-- END -----------------------\n";
          expr_body
        | If (bexp, t0, t1) ->
          let if_ = self#infer_private bexp in
          let then_ = self#infer_private t0 in
          let else_ = self#infer_private t1 in
          self#constraint_ if_ type_bool;
          self#constraint_ then_ else_;
          then_

      (* method infer_decl t = *)
      (*   let ret = self#infer t in *)
      (*   failwith "wip"; *)
      (*   () *)
    end

  module Solver : sig
    type 'a solve = (typecheck_error, 'a) Either.t
    type 'a inferred = (typecheck_error * Ast.term, 'a) Either.t

    val run_solve : constraints -> subst solve
    val run_infer : infer -> Ast.term -> polytype inferred
    val run_infer_module : infer -> Ast.term list -> (polytype * Ast.term) inferred list
  end = struct
    type 'a solve = (typecheck_error, 'a) Either.t
    type 'a inferred = (typecheck_error * Ast.term, 'a) Either.t

    (*
       Can fail with Type_mismatch
    *)
    let rec unify : ltype -> ltype -> subst solve =
      fun a b ->
      match a, b with
      | a, b when a = b -> Right TypeMap.empty
      | TypVar tv, t -> bind tv t
      | t, TypVar tv -> bind tv t
      (*--------------------------------------------------*)
      | Typ (n0, _), Typ (n1, _) when n0 != n1 ->
        Left (Type_mismatch (LType.string_of_ltype a, LType.string_of_ltype b))
      | Typ (_, rest0), Typ (_, rest1) -> unify_many rest0 rest1

    and unify_many a b =
      match a, b with
      | [], [] -> Right TypeMap.empty
      | hd0 :: tl0, hd1 :: tl1 ->
        let s1_ = unify hd0 hd1 in
        (match s1_ with
         | Left err -> Left err
         | Right s1 ->
           let s2_ = unify_many (List.map (apply s1) tl0) (List.map (apply s1) tl1) in
           (match s2_ with
            | Left err -> Left err
            | Right s2 -> Right (compose s2 s1)))
      | _, _ -> failwith "I dont know what should happen in this case"

    and bind : ltypevar -> ltype -> subst solve =
      fun v t ->
      match TypeSet.find_opt v (tvs t) with
      | Some x -> Left (Infinite_type (LType.string_of_ltypevar x))
      | None -> Right (TypeMap.singleton v t)
    ;;

    let rec solve : subst -> constraints -> subst solve =
      fun s c ->
      match c with
      | [] -> Right s
      | (c1, c2) :: tl ->
        (match unify c1 c2 with
         | Left err -> Left err
         | Right s1 -> solve (compose s1 s) (List.map (apply_constraint s1) tl))
    ;;

    let run_solve : constraints -> (typecheck_error, subst) Either.t = solve TypeMap.empty

    let run_infer : infer -> Ast.term -> polytype inferred =
      fun i term ->
      let type_, c = i#infer term in
      match run_solve c with
      | Left err -> Left (err, term)
      | Right subst ->
        let infer_type = apply subst type_ in
        Right (i#generalize infer_type)
    ;;

    let modify_infer_decl : infer -> Ast.term -> polytype -> infer =
      fun infer t typ ->
      match t with
      | Decl (name, _) -> infer#copy_with (SMap.add name typ)
      | _ -> infer
    ;;

    let ( >>= ) e f =
      let open Either in
      match e with
      | Left err -> Left err
      | Right v -> f v
    ;;

    (* .
      let x = 10
      let main = let foo = (λx.λy . y x) x
        in foo 10

      should not typecheck but it does
    *)
    let run_infer_module : infer -> Ast.term list -> (polytype * Ast.term) inferred list =
      fun i decls ->
      let ref_i = ref i in
      let open Either in
      List.rev
      @@ List.fold_left
           (fun acc term ->
              ((try run_infer !ref_i term with
                | HM_exn err -> Left (err, term))
               >>= fun t ->
               ref_i := modify_infer_decl !ref_i term t;
               Right (t, term))
              :: acc)
           []
           decls
    ;;
  end
end
