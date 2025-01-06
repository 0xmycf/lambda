module Smap : sig
  include module type of Map.Make (String)

  val find_def : key -> 'a -> 'a t -> 'a
  val string_values : (key -> 'a -> 'b) -> 'a t -> 'b list
  val right_union : 'a t -> 'a t -> 'a t
  val left_union : 'a t -> 'a t -> 'a t
end

module Sset : module type of Set.Make (String)

type typecheck_error =
  | Undefined_variable of string
  | Type_mismatch of string * string
  | Infinite_type of string

exception HM_exn of typecheck_error

val string_of_tc_error : typecheck_error -> string

module Type : sig
  type t =
    | TyVar of string
    | TyCon of string * t list

  type typ = t

  (* pretty printing *)
  val show : t -> string
  val pretty_show : t -> string
  val tyvar : string -> t
  val free : t -> Sset.t

  (* built in types *)
  val bool_type : t
  val int_type : t
  val string_type : t
  val lambda_type : t -> t -> t
  val apply : t Smap.t -> t -> t

  module Substitution : sig
    type t = typ Smap.t

    module M = Smap

    val show : typ Smap.t -> string
  end

  val unify : typ -> typ -> Substitution.t
end

module TypeScheme : sig
  type t = Sset.t * Type.t

  module M = Sset

  val show : Sset.t * Type.t -> string
  val poly : Type.t -> t
  val apply : Type.t Smap.t -> Sset.t * Type.t -> Sset.t * Type.t
  val instantiate : Sset.t * Type.t -> Type.t list -> Type.t
  val len : Sset.t * 'a -> int

  module Ops : sig
    val ( -/ ) : M.t -> M.t -> M.t
  end
end

module Environment : sig
  include module type of Smap

  type t = TypeScheme.t Smap.t

  val show : (Sset.t * Type.t) Smap.t -> string
  val apply : Type.t Smap.t -> (Sset.t * Type.t) Smap.t -> (Sset.t * Type.t) Smap.t

  module Ops : sig
    val ( +: ) : 'a Smap.t -> key * 'a -> 'a Smap.t
    val ( +:: ) : TypeScheme.t Smap.t -> key * Type.t -> TypeScheme.t Smap.t
  end
end

module type Algorithm = sig
  type input
  type output

  val infer : Ast.t -> input -> output
  val infer_many : Ast.t list -> Environment.t
end

module type W_type = sig
  type input = Environment.t
  type output = Type.Substitution.t * Type.t

  val infer : Ast.t -> input -> output
  val infer_many : Ast.t list -> Environment.t
  val gen : Environment.t -> Type.t -> TypeScheme.t
end

module W : W_type
