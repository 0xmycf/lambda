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
      match Parser.module_ Lexer.read lexer with
      | ls -> List.iteri (fun i a -> Printf.printf "%i: %s\n" i (show_term a)) ls
      (* | Some t -> Printf.printf "%s\n\n" (show_term t) *)
      (* | None -> () *)
    in
    loop lexer
  with
  | Parser.Error -> failwith ("Parse Error on input: " ^ Lexing.lexeme lexer)
;;

(* let rec loop lexer = *)
(*   flush stdout; *)
(*   let t = Lexer.read lexer in *)
(*   Printf.printf "%s " (show_token t); *)
(*   loop lexer *)
(* ;; *)

let () = loop (Lexing.from_channel stdin)
