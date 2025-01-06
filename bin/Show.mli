module type S = sig
  type t

  val show : t -> string
end

val show
  :  ('a -> string)
  -> ((string -> 'a -> string) -> 'b -> string list)
  -> 'b
  -> string

