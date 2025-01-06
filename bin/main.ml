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

let string_of_tc_error = Typechecker.string_of_tc_error

let string_of_polytype_inferred =
  let open Either in
  function
  | Left (err, term) -> string_of_tc_error err ^ "\nIn term:\t" ^ Ast.show_term term
  | Right (poly, term) ->
    Ast.show_term term ^ " : " ^ Typechecker.TC_types.string_of_polytype poly
;;

let string_of_opt = function
  | None -> "None"
  | Some x -> "Some " ^ x
;;

let print_ast terms =
  List.iter (fun t -> Printf.printf "%s\n" (Ast.show_term t)) terms;
  print_newline ()
;;

let t = ref []

let () =
  let file = Sys.argv.(1) in
  let chan = In_channel.open_bin file in
  let lex = Lexing.from_channel chan in
  try
    let terms = Parser.module_ Lexer.read lex in
    t := terms
  with
  | Parser.Error i ->
    Printf.printf "Parse Error on input:(%i)%s\n" i (Parser_messages.message i)
  | Typechecker.HM_exn e ->
    let module Tc = Typechecker.TC_types in
    let () = Printf.printf "(caught exn)\n\t" in
    print_endline @@ string_of_tc_error e
;;

(* let () = *)
(*   let terms = !t in *)
(*   print_endline "Now the AST:"; *)
(*   List.iter (fun x -> print_endline @@ pretty_print_ast x) terms *)
(* ;; *)

let () =
  print_endline "running new checker";
  let module W = Typechecker.W in
  let module Tc = Typechecker in
  let module Env = Typechecker.Environment in
  let terms = !t in
  (* let env = Env.empty in *)
  (* let go t = *)
  (*   ( (match W.infer t env with *)
  (*      | exception Tc.HM_exn a -> *)
  (*        print_endline @@ string_of_tc_error a ^ " in " ^ Ast.show_term t; *)
  (*        failwith "HM exn" *)
  (*      | ok -> ok) *)
  (*   , t ) *)
  (* in *)
  let env =
    try W.infer_many terms with
    | Failure s ->
      print_endline s;
      failwith "HM_exn"
  in
  print_newline ();
  (* let env' = Env.map (fun (_, s) -> W.gen env s) env
    in *)
  (* let foo = List.map go terms in *)
  (* let env = !ref_env in *)
  List.iter
    (function 
      | (Decl (name, _term)) as decl -> begin
       let tau = Env.find name env in
       Printf.printf "%s : %s \n" (Ast.show_term decl) (Typechecker.TypeScheme.show tau) end 
      | _expr ->  print_endline "unsupported top level expression"; failwith ""
       (* (Tc.TypeScheme.show (W.gen env tau) )*))
    terms;
  print_endline
    "-- DONE ------------------------------------------------------------------------\n"
;;

(* let () = *)
(*   let terms = !t in *)
(*   let module Tc = Typechecker.TC_types in *)
(*   let module Solver = Typechecker.TC_types.Solver in *)
(*   let i = new Tc.infer Tc.SMap.empty in *)
(*   let checks = Solver.run_infer_module i terms in *)
(*   if List.for_all Either.is_right checks *)
(*   then *)
(*     print_endline *)
(*     @@ string_of_opt *)
(*     @@ Option.map *)
(*          Ast.show_term *)
(*          Interpreter.( *)
(*            try *)
(*              let final = reduce_module terms in *)
(*              Option.map *)
(*                (fun term -> reduce final print_endline term) *)
(*                (StringMap.find_opt "main" final) *)
(*            with *)
(*            | Reduce_error s -> *)
(*              print_endline s; *)
(*              exit 1) *)
(*   else List.iter (fun x -> print_endline @@ string_of_polytype_inferred x) checks *)
(* ;; *)

(* let errs = List.filter Either.is_left checks in *)
(* List.iter (fun x -> print_endline @@ string_of_polytype_solve x)  errs; *)
(* print_ast terms *)
