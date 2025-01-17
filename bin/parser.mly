(* here https://twolodzko.github.io/posts/ocaml-parser.html *)
%{
    open Ast
%}

%token <string> LIT
%token L_PAREN          "("
%token R_PAREN          ")"
%token LAMBDA           "λ" (* or "λ" or "\\" *)
%token DOT              "."

/* assignment */
%token LET "let", EQUAL "=", IN "in"
/* conditional */
%token IF "if", THEN "then", ELSE "else", TRUE "true", FALSE "false"

%token <int>INT        "-1" (* example *)
%token PLUS MINUS
%token TIMES DIV

%token EOF

/* list every token that can start an expression see: https://ptival.github.io/2017/05/16/parser-generators-and-function-application/ */

%left LET
%left ELSE

%nonassoc LAMBODY /* the body of a lambda should always include everything */

%left IN

%nonassoc LIT, INT, TRUE, FALSE, EQUAL
%right LAMBDA
%left L_PAREN, IF


/* PEMDAS */
%left PLUS MINUS
%left TIMES DIV

/*
    ensures that f a b is parsed as f<a><b> and not f<a<b>>
*/
%left APP

(* %start term *)
(* %type <Ast.term option> term *)

%start module_
%type <Ast.term list> module_

%%

module_:
    | list(assign); EOF { $1 }

(* term:  *)
(*     | lt = lam_term; EOF *)
(*         { Some lt } *)
(*     | EOF  *)
(*         { None } *)
(*     ; *)

value:
    | LIT                { Lit $1 }
    | INT                { IntLit $1 }
    | TRUE               { Bexp true }
    | FALSE              { Bexp false }
    | arith              { $1 }
    | let_expr           { $1 }
    (* if then else *)
    | ifte               { $1 }
    | "("; lam_term; ")" { $2 }
    ;

ifte:
    | "if"; b = lam_term; "then"; t = lam_term; "else"; e = lam_term
        { If (b, t, e) }
    ;

/*

    let x = 10 in \y . x + y

    (\x . (\y . x + y)) 10

    CONFLICT

    let foo = bar in foo 10

    can be resolved in

    i)  (let foo = bar in foo 10)
    ii) (let foo = bar in foo) 10

*/
let_expr:
    | "let"; name = LIT; "="; t = lam_term; "in"; e = lam_term
        (* sadly this is not compatible with typed lambda calculus und HM-type inference *)
        (* { App (Lam (name, e), t) } (* %prec LET_IN *) *)
        { LetIn (name, t, e)}
    ;

/* let ident = _ 
    
   i.e. a top-level declaration
*/
assign: 
    | "let"; l = LIT; "="; t = lam_term %prec LET
        { Decl (l, t) }
    ;

lam_term:
    | app                { $1 }
    | value              { $1 }
    | fn                 { $1 }
    ;

fn:
    | "λ"; i = LIT; "."; t = lam_term %prec LAMBODY
        { Lam (i, t) } 
    ;

app:
    | hd = lam_term; u = lam_term
        { App (hd, u) } %prec APP
    ;

arith:
    | t1 = lam_term; op; t2 = lam_term 
        { BinOp (t1, $2, t2) }
    ;

%inline op:
    | MINUS { Minus }
    | PLUS  { Plus }
    | TIMES { Times }
    | DIV   { Div }
    ;
