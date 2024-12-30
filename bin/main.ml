open Ast

let show_token t =
  let open Parser in
  match t with
  | L_PAREN -> "L_PAREN"
  | R_PAREN -> "R_PAREN"
  | LAMBDA -> "LAMBDA"
  | DOT -> "DOT"
  | EOF -> "EOF"
  | LIT s -> "LIT(" ^ s ^ ")"
  | INT i -> "INT(" ^ string_of_int i ^ ")"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIV -> "DIV"
  | _ -> failwith "not implemented"
;;

let rec loop lexer =
  try
    flush stdout;
    let _ =
      (* match Parser.module_ Lexer.read lexer with *)
      (* | ls -> List.iteri (fun i a -> Printf.printf "%i: %s\n" i (show_term a)) ls *)
      match Parser.term Lexer.read lexer with
      | Some t ->
        Printf.printf "%s\n" (show_term t);
        let module Tc = Typechecker.TC_types in
        let module Lt = Typechecker.LType in
        let infer = new Tc.infer Tc.SMap.empty in
        let type_ = infer#infer t in
        let c, _ = infer#get_meta in
        let open Printf in
        (match Tc.run_solve c with
         | Left (Tc.Type_mismatch (_a, _b)) ->
           printf "Type mismtach between %s and %s" _a _b
         | Left (Tc.Infinite_type _a) -> printf "Cannot construct infinite type %s" _a
         | Left (Tc.Undefined_variable _a) -> printf "%s is not defined" _a
         | Right s ->
           printf "Type: %s\n\n" (Typechecker.LType.string_of_ltype type_);
           let as_list = Tc.TypeMap.to_list s in
           List.iteri
             (fun i (l, r) ->
                Printf.printf
                  "%i: %s %s\n"
                  i
                  (Lt.string_of_ltype r)
                  (Lt.string_of_ltypevar l))
             as_list;
           printf "Type after substitution %s" (Lt.string_of_ltype (Tc.apply s type_)))
      | None -> ()
    in
    loop lexer
  with
  | Parser.Error ->
    Printf.printf "Parse Error on input:%s " (Lexing.lexeme lexer);
    loop lexer
  | Typechecker.TC_types.HM_exn e ->
    (let module Tc = Typechecker.TC_types in
    let open Printf in
    let () = printf "(caught exn)\n\t" in
    match e with
    | Tc.Type_mismatch (_a, _b) -> printf "Type mismtach between \n\t%s and \n\t%s" _a _b
    | Tc.Infinite_type _a -> printf "Cannot construct infinite type %s" _a
    | Tc.Undefined_variable _a -> printf "%s is not defined" _a);
    loop lexer
;;

(* let rec loop lexer = *)
(*   flush stdout; *)
(*   let t = Lexer.read lexer in *)
(*   Printf.printf "%s " (show_token t); *)
(*   loop lexer *)
(* ;; *)

let () = loop (Lexing.from_channel stdin)
