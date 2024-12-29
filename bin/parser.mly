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

%nonassoc LAMBODY
%left PLUS MINUS
%left TIMES DIV
%nonassoc LIT, L_PAREN, INT, LAMBDA
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
    | "("; lam_term; ")" 
        { $2 }
    | x = app
        { x }
    | v = value
        { v }
    | f = fn 
        { f }
    ;

fn:
    | LAMBDA; i = LIT; "."; t = lam_term
        { Lam (i, t) } %prec LAMBODY
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
