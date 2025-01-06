open Ast

(* see commit c32ae733d2fb31ce005ffea3a6ff5aafc835a4aa 
  for the parsing debug hack
*)

let t = ref []

let read chan =
  let lex = Lexing.from_channel chan in
  try
    let terms = Parser.module_ Lexer.read lex in
    t := terms
  with
  | Parser.Error i ->
    Printf.printf "Parse Error on input:(%i)%s\n" i (Parser_messages.message i)
;;

let () =
  let file = Sys.argv.(1) in
  let chan = In_channel.open_bin file in
  Fun.protect (fun _ -> read chan) ~finally:(fun () -> In_channel.close chan)
;;

let () =
  let module W = Typechecker.W in
  let module Tc = Typechecker in
  let module Env = Typechecker.Environment in
  let terms = !t in
  let env =
    try W.infer_many terms with
    | Failure s ->
      print_endline s;
      failwith "HM_exn"
  in
  print_newline ();
  List.iter
    (function
      | Decl (name, _term) as decl ->
        let tau = Env.find name env in
        Printf.printf "%s : %s \n" (Ast.show_term decl) (Typechecker.TypeScheme.show tau)
      | expr ->
        print_endline "unsupported top level expression";
        failwith (Ast.show_term expr ^ " is not supported"))
    terms;
  print_endline
    "-- DONE ------------------------------------------------------------------------\n"
;;
