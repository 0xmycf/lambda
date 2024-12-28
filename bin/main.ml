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
;;

let rec loop lexer =
  flush stdout;
  let _ = match Parser.term Lexer.read lexer with
  | Some t ->
      Printf.printf "%s\n\n" (show_term t)
  | None -> () in
  loop lexer

(* let rec loop lexer = *)
(*   flush stdout; *)
(*   let t = Lexer.read lexer in *)
(*   Printf.printf "%s " (show_token t); *)
(*   loop lexer *)
(* ;; *)

let () = loop (Lexing.from_channel stdin)
