module type S = sig
  type t

  val show : t -> string
end

let show fn map sub =
  let intermediate = map (fun key a -> "\t" ^ key ^ ": " ^ fn a) sub in
  let values = String.concat ", \n" intermediate in
  "{ " ^ values ^ " }"
;;
