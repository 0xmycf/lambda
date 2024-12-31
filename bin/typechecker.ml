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

    let rec apply_constraint : subst -> constraint_ -> constraint_ =
      fun s (c1, c2) -> apply s c1, apply s c2
    ;;

    let compose : subst -> subst -> subst =
      fun s0 s1 ->
      let ( +/ ) = TypeMap.union (fun _ a _ -> Some a) in
      TypeMap.map (apply s0) (s0 +/ s1)
    ;;
  end

  type typecheck_error =
    | Undefined_variable of string
    | Type_mismatch of string * string
    | Infinite_type of string

  exception HM_exn of typecheck_error

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
        | App (t0, t1) ->
          let applicant = self#infer_private t0 in
          let arg = self#infer_private t1 in
          let ret = self#fresh in
          self#constraint_ applicant (LType.lambda arg ret);
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
        | LetIn (str, t0, t1) ->
          let rhs = self#infer_private t0 in
          let gen = self#generalize rhs in
          let expr_body = self#run_with_copy (SMap.add str gen) t1 in
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

    val run_solve : constraints -> subst solve
    val run_infer : infer -> Ast.term -> polytype solve
    val run_infer_module : infer -> Ast.term list -> polytype solve list
  end = struct
    type 'a solve = (typecheck_error, 'a) Either.t

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

    let run_infer : infer -> Ast.term -> polytype solve =
      fun i typ ->
      let type_, c = i#infer typ in
      match run_solve c with
      | Left err -> Left err
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

    let run_infer_module : infer -> Ast.term list -> polytype solve list =
      fun i decls ->
      let ref_i = ref i in
      let open Either in
      List.rev
      @@ List.fold_left
           (fun acc v ->
              ((try run_infer !ref_i v with
                | HM_exn err -> Left err)
               >>= fun t ->
               ref_i := modify_infer_decl !ref_i v t;
               Right t)
              :: acc)
           []
           decls
    ;;
  end
end
