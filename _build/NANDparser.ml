type token =
  | EOF
  | VAR_ID of (PL_functor.varID)
  | NAND
  | ASG
  | COMMA
  | CONST of (PL_functor.bit)
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACK
  | RIGHT_BRACK
  | DEF
  | FUNC_ID of (PL_functor.funcID)
  | IF

open Parsing;;
let _ = parse_error;;
# 2 "NANDparser.mly"
open PL_functor;;
 
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
# 36 "NANDparser.ml"
let yytransl_const = [|
    0 (* EOF *);
  258 (* NAND *);
  259 (* ASG *);
  260 (* COMMA *);
  262 (* LEFT_PAREN *);
  263 (* RIGHT_PAREN *);
  264 (* LEFT_BRACK *);
  265 (* RIGHT_BRACK *);
  266 (* DEF *);
  268 (* IF *);
    0|]

let yytransl_block = [|
  257 (* VAR_ID *);
  261 (* CONST *);
  267 (* FUNC_ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\005\000\005\000\
\006\000\006\000\006\000\006\000\006\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\003\000\010\000\007\000\003\000\001\000\
\003\000\001\000\001\000\004\000\003\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\002\000\000\000\014\000\
\000\000\010\000\011\000\000\000\000\000\000\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\009\000\000\000\007\000\000\000\012\000\000\000\000\000\
\006\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\009\000\023\000\024\000"

let yysindex = "\011\000\
\005\255\000\000\016\255\027\255\024\255\000\000\031\000\005\255\
\029\255\027\255\030\255\013\255\000\000\000\000\013\255\000\000\
\025\255\000\000\000\000\013\255\031\255\002\255\000\000\021\255\
\032\255\009\255\013\255\013\255\033\255\013\255\027\255\000\000\
\028\255\000\000\005\255\000\000\035\255\000\000\034\255\036\255\
\000\000\005\255\037\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\019\255\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\248\255\000\000\254\255\236\255\249\255"

let yytablesize = 269
let yytable = "\014\000\
\008\000\011\000\003\000\028\000\022\000\003\000\033\000\016\000\
\029\000\036\000\028\000\001\000\026\000\018\000\004\000\032\000\
\005\000\019\000\020\000\010\000\034\000\015\000\028\000\021\000\
\030\000\015\000\039\000\003\000\037\000\012\000\013\000\015\000\
\017\000\043\000\038\000\025\000\027\000\031\000\000\000\000\000\
\035\000\040\000\041\000\042\000\000\000\044\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\000\000\000\000\000\000\000\000\000\000\008\000\
\000\000\008\000\008\000\003\000\008\000"

let yycheck = "\008\000\
\000\000\004\000\000\000\002\001\012\000\001\001\027\000\010\000\
\007\001\030\000\002\001\001\000\020\000\001\001\010\001\007\001\
\012\001\005\001\006\001\004\001\028\000\003\001\002\001\011\001\
\004\001\007\001\035\000\001\001\031\000\006\001\000\000\003\001\
\003\001\042\000\007\001\011\001\006\001\006\001\255\255\255\255\
\008\001\007\001\009\001\008\001\255\255\009\001\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\255\255\255\255\255\255\007\001\
\255\255\009\001\010\001\009\001\012\001"

let yynames_const = "\
  EOF\000\
  NAND\000\
  ASG\000\
  COMMA\000\
  LEFT_PAREN\000\
  RIGHT_PAREN\000\
  LEFT_BRACK\000\
  RIGHT_BRACK\000\
  DEF\000\
  IF\000\
  "

let yynames_block = "\
  VAR_ID\000\
  CONST\000\
  FUNC_ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nandProg) in
    Obj.repr(
# 39 "NANDparser.mly"
                        ( _1 )
# 193 "NANDparser.ml"
               : PL_functor.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nandCom) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nandProg) in
    Obj.repr(
# 41 "NANDparser.mly"
                           (_1 :: _2)
# 201 "NANDparser.ml"
               : 'nandProg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nandCom) in
    Obj.repr(
# 42 "NANDparser.mly"
               ([_1])
# 208 "NANDparser.ml"
               : 'nandProg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 45 "NANDparser.mly"
                 (Asg(_1, _3))
# 216 "NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'ids) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : PL_functor.funcID) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'ids) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'nandProg) in
    Obj.repr(
# 47 "NANDparser.mly"
       ( FxnDef((_4, {inputs = _6; outputs = _2; body = _9 })) )
# 226 "NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'nandProg) in
    Obj.repr(
# 49 "NANDparser.mly"
       ( If(_3, _6) )
# 234 "NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 51 "NANDparser.mly"
                   ( _1 :: _3 )
# 242 "NANDparser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 52 "NANDparser.mly"
        ( [_1] )
# 249 "NANDparser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 55 "NANDparser.mly"
                  ( Nand(_1, _3) )
# 257 "NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : PL_functor.varID) in
    Obj.repr(
# 56 "NANDparser.mly"
            ( (*  (checkReadId $1); *)  Var(_1) )
# 264 "NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : PL_functor.bit) in
    Obj.repr(
# 57 "NANDparser.mly"
           ( Const(_1) )
# 271 "NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : PL_functor.funcID) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exps) in
    Obj.repr(
# 59 "NANDparser.mly"
       ( FxnApp(_1, _3) )
# 279 "NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 60 "NANDparser.mly"
                                ( _2 )
# 286 "NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : PL_functor.varID) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ids) in
    Obj.repr(
# 62 "NANDparser.mly"
                     ((*(checkWriteId $1);*) _1 :: _3 )
# 294 "NANDparser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : PL_functor.varID) in
    Obj.repr(
# 63 "NANDparser.mly"
           ((* checkWriteId $1 ;*)  [_1] )
# 301 "NANDparser.ml"
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
