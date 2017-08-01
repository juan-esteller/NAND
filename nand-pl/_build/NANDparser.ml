type token =
  | EOF
  | VAR_ID of (PL_functor.varID)
  | BINOP of (Binops.binop)
  | INDEXOP of (PL_functor.indexop)
  | ASG
  | COMMA
  | CONST of (int)
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACK
  | RIGHT_BRACK
  | DEF
  | WHILE
  | FUNC_ID of (PL_functor.funcID)
  | IF

open Parsing;;
let _ = parse_error;;
# 2 "NANDparser.mly"
open PL_functor;;
open Binops ;; 
open Indexops ;; 

exception Invalid_operation
 
let writeOnly = ["y"; "loop"]
let readOnly = ["x"]

exception Invalid_var of string 

let checkReadId (id: varID) : unit = 
  let body, ind = id in 
    if List.mem body writeOnly then 
       raise (Invalid_var(strOfId id)) 

let checkWriteId (id: varID) : unit = 
  let body, ind = id in 
    if List.mem body readOnly then 
       raise (Invalid_var(strOfId id))  
# 42 "NANDparser.ml"
let yytransl_const = [|
    0 (* EOF *);
  260 (* ASG *);
  261 (* COMMA *);
  263 (* LEFT_PAREN *);
  264 (* RIGHT_PAREN *);
  265 (* LEFT_BRACK *);
  266 (* RIGHT_BRACK *);
  267 (* DEF *);
  268 (* WHILE *);
  270 (* IF *);
    0|]

let yytransl_block = [|
  257 (* VAR_ID *);
  258 (* BINOP *);
  259 (* INDEXOP *);
  262 (* CONST *);
  269 (* FUNC_ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\005\000\005\000\006\000\006\000\006\000\006\000\006\000\004\000\
\004\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\003\000\010\000\007\000\007\000\002\000\
\003\000\001\000\003\000\001\000\001\000\004\000\003\000\003\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\001\000\002\000\000\000\016\000\000\000\012\000\013\000\000\000\
\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\015\000\000\000\011\000\
\000\000\000\000\009\000\000\000\014\000\000\000\000\000\000\000\
\007\000\006\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\028\000\029\000"

let yysindex = "\010\000\
\006\255\000\000\033\255\025\255\005\255\023\255\000\000\037\000\
\006\255\035\255\000\000\025\255\036\255\039\255\002\255\002\255\
\000\000\000\000\002\255\000\000\027\255\000\000\000\000\002\255\
\037\255\011\255\014\255\000\000\029\255\038\255\019\255\002\255\
\002\255\040\255\041\255\002\255\025\255\000\000\043\255\000\000\
\006\255\006\255\000\000\044\255\000\000\045\255\046\255\048\255\
\000\000\000\000\006\255\049\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\042\255\000\000\000\000\000\000\000\000\000\000\
\004\000\000\000\000\000\000\000\020\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\247\255\000\000\254\255\249\255\246\255"

let yytablesize = 271
let yytable = "\018\000\
\010\000\014\000\022\000\003\000\026\000\027\000\003\000\023\000\
\024\000\020\000\001\000\015\000\033\000\031\000\025\000\033\000\
\004\000\005\000\034\000\006\000\033\000\035\000\040\000\017\000\
\039\000\013\000\038\000\017\000\043\000\016\000\033\000\046\000\
\047\000\036\000\044\000\011\000\017\000\012\000\019\000\030\000\
\012\000\052\000\021\000\032\000\037\000\017\000\000\000\000\000\
\041\000\042\000\045\000\048\000\000\000\000\000\049\000\050\000\
\051\000\000\000\053\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\000\000\000\010\000\010\000\010\000\003\000\010\000"

let yycheck = "\009\000\
\000\000\004\000\001\001\000\000\015\000\016\000\001\001\006\001\
\007\001\012\000\001\000\007\001\002\001\024\000\013\001\002\001\
\011\001\012\001\008\001\014\001\002\001\008\001\033\000\004\001\
\032\000\001\001\008\001\008\001\036\000\007\001\002\001\041\000\
\042\000\005\001\037\000\003\001\000\000\005\001\004\001\013\001\
\005\001\051\000\004\001\007\001\007\001\004\001\255\255\255\255\
\009\001\009\001\008\001\008\001\255\255\255\255\010\001\010\001\
\009\001\255\255\010\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\255\255\255\255\255\255\255\255\
\008\001\255\255\010\001\011\001\012\001\010\001\014\001"

let yynames_const = "\
  EOF\000\
  ASG\000\
  COMMA\000\
  LEFT_PAREN\000\
  RIGHT_PAREN\000\
  LEFT_BRACK\000\
  RIGHT_BRACK\000\
  DEF\000\
  WHILE\000\
  IF\000\
  "

let yynames_block = "\
  VAR_ID\000\
  BINOP\000\
  INDEXOP\000\
  CONST\000\
  FUNC_ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nandProg) in
    Obj.repr(
# 45 "NANDparser.mly"
                        ( _1 )
# 208 "NANDparser.ml"
               : PL_functor.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nandCom) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nandProg) in
    Obj.repr(
# 47 "NANDparser.mly"
                           (_1 :: _2)
# 216 "NANDparser.ml"
               : 'nandProg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nandCom) in
    Obj.repr(
# 48 "NANDparser.mly"
               ([_1])
# 223 "NANDparser.ml"
               : 'nandProg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 51 "NANDparser.mly"
                 (Asg(_1, _3))
# 231 "NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'ids) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : PL_functor.funcID) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'ids) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'nandProg) in
    Obj.repr(
# 53 "NANDparser.mly"
       ( FxnDef((_4, {inputs = _6; outputs = _2; body = _9 })) )
# 241 "NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'nandProg) in
    Obj.repr(
# 55 "NANDparser.mly"
       ( If(_3, _6) )
# 249 "NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'nandProg) in
    Obj.repr(
# 57 "NANDparser.mly"
       ( While(_3, _6) )
# 257 "NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : PL_functor.varID) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : PL_functor.indexop) in
    Obj.repr(
# 59 "NANDparser.mly"
       ( let bod, ind = _1 in 
           if bod <> "i" || ind <> Int(0) then 
             raise (Invalid_operation) 
           else 
             IndexOp(_2) )
# 269 "NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 65 "NANDparser.mly"
                   ( _1 :: _3 )
# 277 "NANDparser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 66 "NANDparser.mly"
        ( [_1] )
# 284 "NANDparser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Binops.binop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 69 "NANDparser.mly"
                    ( Binop(_2, _1, _3) )
# 293 "NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : PL_functor.varID) in
    Obj.repr(
# 70 "NANDparser.mly"
            ( (*  (checkReadId $1); *)  Var(_1) )
# 300 "NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 71 "NANDparser.mly"
           ( Const(_1) )
# 307 "NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : PL_functor.funcID) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exps) in
    Obj.repr(
# 73 "NANDparser.mly"
       ( FxnApp(_1, _3) )
# 315 "NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 74 "NANDparser.mly"
                                ( _2 )
# 322 "NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : PL_functor.varID) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ids) in
    Obj.repr(
# 76 "NANDparser.mly"
                     ((*(checkWriteId $1);*) _1 :: _3 )
# 330 "NANDparser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : PL_functor.varID) in
    Obj.repr(
# 77 "NANDparser.mly"
           ((* checkWriteId $1 ;*)  [_1] )
# 337 "NANDparser.ml"
               : 'ids))
(* Entry parseProg *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parseProg (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : PL_functor.program)
