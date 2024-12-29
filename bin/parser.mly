(* here https://twolodzko.github.io/posts/ocaml-parser.html *)
%{
    open Ast
%}

%token <string> LIT
%token L_PAREN          "("
%token R_PAREN          ")"
%token LAMBDA           "λ" (* or "λ" or "\\" *)
%token DOT              "."

%token <int>INT        "-1" (* example *)
%token PLUS MINUS
%token TIMES DIV

%token EOF

%nonassoc LAMBODY /* the body of a lambda should always include everything  */
/* PEMDAS */
%left PLUS MINUS
%left TIMES DIV

/*
    ensures that f a b is parsed as f<a><b> and not f<a<b>>
*/
%nonassoc LIT, L_PAREN, INT, LAMBDA /* list every token that can start an expression see: https://ptival.github.io/2017/05/16/parser-generators-and-function-application/ */
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
    | LIT   { Lit $1 }
    | INT   { IntLit $1 }
    | arith { $1 }
    ;

lam_term:
    | "("; lam_term; ")" { $2 }
    | app                { $1 }
    | value              { $1 }
    | fn                 { $1 }
    ;

fn:
    | "λ"; i = LIT; "."; t = lam_term
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
