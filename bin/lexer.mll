{

open Lexing
open Parser

exception SyntaxError of string
exception UnmatchedBinOp

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let white = [' ' '\t']+
let newline = '\n' | '\t' | "\r\t"
let id_suffix = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let id = ['a'-'z' '_'] id_suffix*

let upcaseID = ['A'-'Z']id_suffix*
let numberID = ['0'-'9']id_suffix+

let dot = '.'

let int = ('-'?['1'-'9']['0'-'9']* | '0') 

let binop = '*' | '+' | '-' | '/' 

let lambda = "lambda" | "\\" | "λ"

rule read =
  parse
  | eof      { EOF }
  | newline  { next_line lexbuf; read lexbuf }
  | white    { read lexbuf }
  | lambda   { LAMBDA }
  | id       { LIT (Lexing.lexeme lexbuf) }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf))}
  | binop    { match (Lexing.lexeme lexbuf) with 
                | "*" -> TIMES
                | "+" -> PLUS
                | "-" -> MINUS
                | "/" -> DIV
                | _ -> raise UnmatchedBinOp
             }
  | dot      { DOT }
  | '('      { L_PAREN }
  | ')'      { R_PAREN }
  | upcaseID
    { raise (SyntaxError "identifiers must start with a lower case letter (not uppercase)")}
  | numberID
    { raise (SyntaxError "identifiers must start with a lower case letter (not a number)")}
  (* | _ { raise (SyntaxError ( "Unexpected syntax " ^ Lexing.lexeme lexbuf )) } *)
