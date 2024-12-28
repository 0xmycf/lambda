(* here https://twolodzko.github.io/posts/ocaml-parser.html *)
%{
    open Ast
%}

%token <string> LIT
%token L_PAREN          "("
%token R_PAREN          ")"
%token LAMBDA           "lam" (* or "λ" or "\\" *)
%token DOT              "."

%token <int>INT        "-1" (* example *)
%token PLUS MINUS
%token TIMES DIV

%token EOF

%nonassoc LAMBODY (* ig this works ? *)
%left PLUS MINUS
%left TIMES DIV
%left APP

%start term
%type <Ast.term option> term
%%

term: 
    | lt = lam_term; EOF
        { Some lt }
    | EOF 
        { None }
    ;

value:
    | x = LIT 
        { Lit x }
    | i = INT
        { IntLit i }
    | a_expr = arith
        { a_expr }
    ;

lam_term:
    | "("; lt = lam_term; ")"
        { lt }
    | v = value
        { v }
    | f = fn 
        { f }
    | x = app 
        { x }
    ;

fn:
    | LAMBDA; i = LIT; "."; t = lam_term
        { Lam (i, t) } %prec LAMBODY
    ;

app:
    | hd = hd; u = lam_term
        { App (hd, u) } %prec APP
    ;

hd:
    | x = LIT
        { Lit x }
    | "("; lt = lam_term; ")"
        { lt }
    ;

/*
    This leads to ambiguity

    \ x . x + x

    is what?

    (\x . x) + x

    or 

    (\x . x + x)
*/

arith:
    | t1 = lam_term; MINUS; t2 = lam_term
        { BinOp (t1, Minus, t2) }
    | t1 = lam_term; PLUS; t2 = lam_term
        { BinOp (t1, Plus, t2) }
    | t1 = lam_term; TIMES; t2 = lam_term
        { BinOp (t1, Times, t2) }
    | t1 = lam_term; DIV; t2 = lam_term
        { BinOp (t1, Div, t2) }
    ;
