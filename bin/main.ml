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

(* let rec loop lexer = *)
(*   try *)
(*     flush stdout; *)
(*     let _ = *)
(*       (* match Parser.module_ Lexer.read lexer with *)
(*       | ls -> List.iteri (fun i a -> Printf.printf "%i: %s\n" i (show_term a)) ls *) *)
(*       match Parser.term Lexer.read lexer with *)
(*       | Some t -> *)
(*         Printf.printf "%s\n" (show_term t); *)
(*         let module Tc = Typechecker.TC_types in *)
(*         let module Lt = Typechecker.LType in *)
(*         let infer = new Tc.infer Tc.SMap.empty in *)
(*         let open Printf in *)
(*         (match Tc.Solver.run_infer infer t with *)
(*          | Left (Tc.Type_mismatch (_a, _b)) -> *)
(*            printf "Type mismtach between \n(got)\t%s\n(need)\t%s\n" _a _b *)
(*          | Left (Tc.Infinite_type _a) -> printf "Cannot construct infinite type %s" _a *)
(*          | Left (Tc.Undefined_variable _a) -> printf "%s is not defined" _a *)
(*          | Right type_ -> *)
(*            printf "Type after substitution:\n\t%s\n\n" (Tc.string_of_polytype type_)) *)
(*       | None -> () *)
(*     in *)
(*     loop lexer *)
(*   with *)
(*   | Parser.Error -> *)
(*     Printf.printf "Parse Error on input:%s\n" (Lexing.lexeme lexer); *)
(*     loop lexer *)
(*   | Typechecker.TC_types.HM_exn e -> *)
(*     (let module Tc = Typechecker.TC_types in *)
(*     let open Printf in *)
(*     let () = printf "(caught exn)\n\t" in *)
(*     match e with *)
(*     | Tc.Type_mismatch (_a, _b) -> *)
(*       printf "Type mismtach between \n(got)\t%s\n(need)\t%s\n" _a _b *)
(*     | Tc.Infinite_type _a -> printf "Cannot construct infinite type %s" _a *)
(*     | Tc.Undefined_variable _a -> printf "%s is not defined" _a); *)
(*     loop lexer *)
(* ;; *)

(* let () = loop (Lexing.from_channel stdin) *)

let string_of_tc_error e =
  let module Tc = Typechecker.TC_types in
  let open Printf in
  "error: " ^ match e with
  | Tc.Type_mismatch (_a, _b) ->
    sprintf "Type mismtach between \n(got)\t%s\n(need)\t%s\n" _a _b
  | Tc.Infinite_type _a -> sprintf "Cannot construct infinite type %s" _a
  | Tc.Undefined_variable _a -> sprintf "%s is not defined" _a
;;

let string_of_polytype_solve =
  let open Either in
  function
  | Left (err : Typechecker.TC_types.typecheck_error) -> string_of_tc_error err
  | Right poly -> Typechecker.TC_types.string_of_polytype poly
;;

let () =
  let chan = In_channel.open_bin "./test/module.lambda" in
  let lex = Lexing.from_channel chan in
  try
    let terms = Parser.module_ Lexer.read lex in
    let () =
      List.iter (fun t -> Printf.printf "%s\n" (Ast.show_term t)) terms;
      print_newline ()
    in
    let module Tc = Typechecker.TC_types in
    let module Solver = Typechecker.TC_types.Solver in
    let i = new Tc.infer Tc.SMap.empty in
    let foo = Solver.run_infer_module i terms in
    List.iter (fun x -> print_endline @@ string_of_polytype_solve x) foo
  with
  | Parser.Error i -> Printf.printf "Parse Error on input:(%i)%s\n" i (Parser_messages.message i)
  | Typechecker.TC_types.HM_exn e ->
    let module Tc = Typechecker.TC_types in
    let () = Printf.printf "(caught exn)\n\t" in
    print_endline @@ string_of_tc_error e
;;
