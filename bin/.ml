
type token = 
  | TIMES
  | R_PAREN
  | PLUS
  | MINUS
  | L_PAREN
  | LIT of (
# 6 "bin/parser.mly"
       (string)
# 12 "bin/.ml"
)
  | LAMBDA
  | INT of (
# 12 "bin/parser.mly"
       (int)
# 18 "bin/.ml"
)
  | EOF
  | DOT
  | DIV

# 2 "bin/parser.mly"
  
    open Ast

# 28 "bin/.ml"

let menhir_begin_marker =
  0

and (xv_value, xv_term, xv_lam_term, xv_hd, xv_fn, xv_arith, xv_app) =
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 39 "bin/parser.mly"
      a_expr
# 37 "bin/.ml"
   : 'tv_arith) (_startpos_a_expr_ : Lexing.position) (_endpos_a_expr_ : Lexing.position) (_startofs_a_expr_ : int) (_endofs_a_expr_ : int) (_loc_a_expr_ : Lexing.position * Lexing.position) : 'tv_value ->
    
# 40 "bin/parser.mly"
        ( a_expr )
# 42 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 37 "bin/parser.mly"
      i
# 47 "bin/.ml"
   : (
# 12 "bin/parser.mly"
       (int)
# 51 "bin/.ml"
  )) (_startpos_i_ : Lexing.position) (_endpos_i_ : Lexing.position) (_startofs_i_ : int) (_endofs_i_ : int) (_loc_i_ : Lexing.position * Lexing.position) : 'tv_value ->
    
# 38 "bin/parser.mly"
        ( IntLit i )
# 56 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 35 "bin/parser.mly"
      x
# 61 "bin/.ml"
   : (
# 6 "bin/parser.mly"
       (string)
# 65 "bin/.ml"
  )) (_startpos_x_ : Lexing.position) (_endpos_x_ : Lexing.position) (_startofs_x_ : int) (_endofs_x_ : int) (_loc_x_ : Lexing.position * Lexing.position) : 'tv_value ->
    
# 36 "bin/parser.mly"
        ( Lit x )
# 70 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 33 "bin/parser.mly"
                     _3
# 75 "bin/.ml"
   : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 33 "bin/parser.mly"
           t
# 79 "bin/.ml"
   : 'tv_value) (_startpos_t_ : Lexing.position) (_endpos_t_ : Lexing.position) (_startofs_t_ : int) (_endofs_t_ : int) (_loc_t_ : Lexing.position * Lexing.position) (
# 33 "bin/parser.mly"
     _1
# 83 "bin/.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_value ->
    
# 34 "bin/parser.mly"
        ( t )
# 88 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 28 "bin/parser.mly"
     _1
# 93 "bin/.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : (
# 22 "bin/parser.mly"
      (Ast.term option)
# 97 "bin/.ml"
  ) ->
    (
# 29 "bin/parser.mly"
        ( None )
# 102 "bin/.ml"
     : 'tv_term) in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 26 "bin/parser.mly"
                    _2
# 107 "bin/.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 26 "bin/parser.mly"
      lt
# 111 "bin/.ml"
   : 'tv_lam_term) (_startpos_lt_ : Lexing.position) (_endpos_lt_ : Lexing.position) (_startofs_lt_ : int) (_endofs_lt_ : int) (_loc_lt_ : Lexing.position * Lexing.position) : (
# 22 "bin/parser.mly"
      (Ast.term option)
# 115 "bin/.ml"
  ) ->
    (
# 27 "bin/parser.mly"
        ( Some lt )
# 120 "bin/.ml"
     : 'tv_term) in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 48 "bin/parser.mly"
      x
# 125 "bin/.ml"
   : 'tv_app) (_startpos_x_ : Lexing.position) (_endpos_x_ : Lexing.position) (_startofs_x_ : int) (_endofs_x_ : int) (_loc_x_ : Lexing.position * Lexing.position) : 'tv_lam_term ->
    
# 49 "bin/parser.mly"
        ( x )
# 130 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 46 "bin/parser.mly"
      f
# 135 "bin/.ml"
   : 'tv_fn) (_startpos_f_ : Lexing.position) (_endpos_f_ : Lexing.position) (_startofs_f_ : int) (_endofs_f_ : int) (_loc_f_ : Lexing.position * Lexing.position) : 'tv_lam_term ->
    
# 47 "bin/parser.mly"
        ( f )
# 140 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 44 "bin/parser.mly"
      v
# 145 "bin/.ml"
   : 'tv_value) (_startpos_v_ : Lexing.position) (_endpos_v_ : Lexing.position) (_startofs_v_ : int) (_endofs_v_ : int) (_loc_v_ : Lexing.position * Lexing.position) : 'tv_lam_term ->
    
# 45 "bin/parser.mly"
        ( v )
# 150 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 65 "bin/parser.mly"
                         _3
# 155 "bin/.ml"
   : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 65 "bin/parser.mly"
           lt
# 159 "bin/.ml"
   : 'tv_lam_term) (_startpos_lt_ : Lexing.position) (_endpos_lt_ : Lexing.position) (_startofs_lt_ : int) (_endofs_lt_ : int) (_loc_lt_ : Lexing.position * Lexing.position) (
# 65 "bin/parser.mly"
     _1
# 163 "bin/.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_hd ->
    
# 66 "bin/parser.mly"
        ( lt )
# 168 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 63 "bin/parser.mly"
      x
# 173 "bin/.ml"
   : (
# 6 "bin/parser.mly"
       (string)
# 177 "bin/.ml"
  )) (_startpos_x_ : Lexing.position) (_endpos_x_ : Lexing.position) (_startofs_x_ : int) (_endofs_x_ : int) (_loc_x_ : Lexing.position * Lexing.position) : 'tv_hd ->
    
# 64 "bin/parser.mly"
        ( Lit x )
# 182 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 53 "bin/parser.mly"
                            t
# 187 "bin/.ml"
   : 'tv_lam_term) (_startpos_t_ : Lexing.position) (_endpos_t_ : Lexing.position) (_startofs_t_ : int) (_endofs_t_ : int) (_loc_t_ : Lexing.position * Lexing.position) (
# 53 "bin/parser.mly"
                      _3
# 191 "bin/.ml"
   : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 53 "bin/parser.mly"
              i
# 195 "bin/.ml"
   : (
# 6 "bin/parser.mly"
       (string)
# 199 "bin/.ml"
  )) (_startpos_i_ : Lexing.position) (_endpos_i_ : Lexing.position) (_startofs_i_ : int) (_endofs_i_ : int) (_loc_i_ : Lexing.position * Lexing.position) (
# 53 "bin/parser.mly"
     _1
# 203 "bin/.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_fn ->
    
# 54 "bin/parser.mly"
        ( Lam (i, t) )
# 208 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 76 "bin/parser.mly"
                          t2
# 213 "bin/.ml"
   : 'tv_lam_term) (_startpos_t2_ : Lexing.position) (_endpos_t2_ : Lexing.position) (_startofs_t2_ : int) (_endofs_t2_ : int) (_loc_t2_ : Lexing.position * Lexing.position) (
# 76 "bin/parser.mly"
                    _2
# 217 "bin/.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 76 "bin/parser.mly"
      t1
# 221 "bin/.ml"
   : 'tv_lam_term) (_startpos_t1_ : Lexing.position) (_endpos_t1_ : Lexing.position) (_startofs_t1_ : int) (_endofs_t1_ : int) (_loc_t1_ : Lexing.position * Lexing.position) : 'tv_arith ->
    
# 77 "bin/parser.mly"
        ( BinOp (t1, Div, t2) )
# 226 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 74 "bin/parser.mly"
                            t2
# 231 "bin/.ml"
   : 'tv_lam_term) (_startpos_t2_ : Lexing.position) (_endpos_t2_ : Lexing.position) (_startofs_t2_ : int) (_endofs_t2_ : int) (_loc_t2_ : Lexing.position * Lexing.position) (
# 74 "bin/parser.mly"
                    _2
# 235 "bin/.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 74 "bin/parser.mly"
      t1
# 239 "bin/.ml"
   : 'tv_lam_term) (_startpos_t1_ : Lexing.position) (_endpos_t1_ : Lexing.position) (_startofs_t1_ : int) (_endofs_t1_ : int) (_loc_t1_ : Lexing.position * Lexing.position) : 'tv_arith ->
    
# 75 "bin/parser.mly"
        ( BinOp (t1, Times, t2) )
# 244 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 72 "bin/parser.mly"
                           t2
# 249 "bin/.ml"
   : 'tv_lam_term) (_startpos_t2_ : Lexing.position) (_endpos_t2_ : Lexing.position) (_startofs_t2_ : int) (_endofs_t2_ : int) (_loc_t2_ : Lexing.position * Lexing.position) (
# 72 "bin/parser.mly"
                    _2
# 253 "bin/.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 72 "bin/parser.mly"
      t1
# 257 "bin/.ml"
   : 'tv_lam_term) (_startpos_t1_ : Lexing.position) (_endpos_t1_ : Lexing.position) (_startofs_t1_ : int) (_endofs_t1_ : int) (_loc_t1_ : Lexing.position * Lexing.position) : 'tv_arith ->
    
# 73 "bin/parser.mly"
        ( BinOp (t1, Plus, t2) )
# 262 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 70 "bin/parser.mly"
                            t2
# 267 "bin/.ml"
   : 'tv_lam_term) (_startpos_t2_ : Lexing.position) (_endpos_t2_ : Lexing.position) (_startofs_t2_ : int) (_endofs_t2_ : int) (_loc_t2_ : Lexing.position * Lexing.position) (
# 70 "bin/parser.mly"
                    _2
# 271 "bin/.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 70 "bin/parser.mly"
      t1
# 275 "bin/.ml"
   : 'tv_lam_term) (_startpos_t1_ : Lexing.position) (_endpos_t1_ : Lexing.position) (_startofs_t1_ : int) (_endofs_t1_ : int) (_loc_t1_ : Lexing.position * Lexing.position) : 'tv_arith ->
    
# 71 "bin/parser.mly"
        ( BinOp (t1, Minus, t2) )
# 280 "bin/.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 58 "bin/parser.mly"
               u
# 285 "bin/.ml"
   : 'tv_lam_term) (_startpos_u_ : Lexing.position) (_endpos_u_ : Lexing.position) (_startofs_u_ : int) (_endofs_u_ : int) (_loc_u_ : Lexing.position * Lexing.position) (
# 58 "bin/parser.mly"
      hd
# 289 "bin/.ml"
   : 'tv_hd) (_startpos_hd_ : Lexing.position) (_endpos_hd_ : Lexing.position) (_startofs_hd_ : int) (_endofs_hd_ : int) (_loc_hd_ : Lexing.position * Lexing.position) : 'tv_app ->
    
# 59 "bin/parser.mly"
        ( App (hd, u) )
# 294 "bin/.ml"
     in
  ((let rec diverge() = diverge() in diverge()) : 'tv_value * 'tv_term * 'tv_lam_term * 'tv_hd * 'tv_fn * 'tv_arith * 'tv_app)

and menhir_end_marker =
  0
