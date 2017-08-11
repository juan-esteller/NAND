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
  | PRINT

open Parsing;;
let _ = parse_error;;
# 2 "parsing/NANDparser.mly"
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
# 43 "parsing/NANDparser.ml"
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
  271 (* PRINT *);
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
\003\000\005\000\005\000\006\000\006\000\006\000\006\000\006\000\
\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\003\000\010\000\007\000\007\000\002\000\
\004\000\003\000\001\000\003\000\001\000\001\000\004\000\003\000\
\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\002\000\000\000\017\000\000\000\013\000\
\014\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
\000\000\016\000\000\000\012\000\000\000\000\000\010\000\000\000\
\015\000\000\000\000\000\000\000\007\000\006\000\000\000\000\000\
\005\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\011\000\031\000\032\000"

let yysindex = "\008\000\
\002\255\000\000\005\255\031\255\012\255\030\255\033\255\000\000\
\038\000\002\255\037\255\000\000\031\255\038\255\040\255\017\255\
\017\255\041\255\000\000\000\000\017\255\000\000\034\255\000\000\
\000\000\017\255\039\255\018\255\019\255\042\255\000\000\029\255\
\044\255\020\255\017\255\017\255\043\255\045\255\000\000\017\255\
\031\255\000\000\047\255\000\000\002\255\002\255\000\000\048\255\
\000\000\049\255\050\255\052\255\000\000\000\000\002\255\053\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\054\255\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\000\000\025\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\246\255\000\000\254\255\228\255\245\255"

let yytablesize = 272
let yytable = "\020\000\
\011\000\015\000\003\000\003\000\028\000\029\000\043\000\012\000\
\001\000\013\000\022\000\047\000\004\000\005\000\034\000\006\000\
\007\000\024\000\016\000\036\000\036\000\036\000\025\000\026\000\
\044\000\037\000\038\000\042\000\018\000\027\000\036\000\014\000\
\018\000\040\000\050\000\051\000\017\000\019\000\048\000\018\000\
\021\000\030\000\013\000\023\000\056\000\035\000\033\000\000\000\
\000\000\039\000\041\000\045\000\000\000\046\000\049\000\052\000\
\000\000\018\000\053\000\054\000\055\000\000\000\057\000\000\000\
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
\000\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\000\000\011\000\011\000\011\000\003\000\011\000\011\000"

let yycheck = "\010\000\
\000\000\004\000\001\001\000\000\016\000\017\000\035\000\003\001\
\001\000\005\001\013\000\040\000\011\001\012\001\026\000\014\001\
\015\001\001\001\007\001\002\001\002\001\002\001\006\001\007\001\
\036\000\008\001\008\001\008\001\004\001\013\001\002\001\001\001\
\008\001\005\001\045\000\046\000\007\001\000\000\041\000\007\001\
\004\001\001\001\005\001\004\001\055\000\007\001\013\001\255\255\
\255\255\008\001\007\001\009\001\255\255\009\001\008\001\008\001\
\255\255\004\001\010\001\010\001\009\001\255\255\010\001\255\255\
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
\008\001\255\255\010\001\011\001\012\001\010\001\014\001\015\001"

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
  PRINT\000\
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
# 47 "parsing/NANDparser.mly"
                        ( _1 )
# 214 "parsing/NANDparser.ml"
               : PL_functor.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nandCom) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nandProg) in
    Obj.repr(
# 49 "parsing/NANDparser.mly"
                           (_1 :: _2)
# 222 "parsing/NANDparser.ml"
               : 'nandProg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nandCom) in
    Obj.repr(
# 50 "parsing/NANDparser.mly"
               ([_1])
# 229 "parsing/NANDparser.ml"
               : 'nandProg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 53 "parsing/NANDparser.mly"
                 (Asg(_1, _3))
# 237 "parsing/NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'ids) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : PL_functor.funcID) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'ids) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'nandProg) in
    Obj.repr(
# 55 "parsing/NANDparser.mly"
       ( FxnDef((_4, {inputs = _6; outputs = _2; body = _9 })) )
# 247 "parsing/NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'nandProg) in
    Obj.repr(
# 57 "parsing/NANDparser.mly"
       ( If(_3, _6) )
# 255 "parsing/NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'nandProg) in
    Obj.repr(
# 59 "parsing/NANDparser.mly"
       ( While(_3, _6) )
# 263 "parsing/NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : PL_functor.varID) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : PL_functor.indexop) in
    Obj.repr(
# 61 "parsing/NANDparser.mly"
       ( let bod, ind = _1 in 
           if bod <> "i" || ind <> Int(0) then 
             raise (Invalid_operation) 
           else 
             IndexOp(_2) )
# 275 "parsing/NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : PL_functor.varID) in
    Obj.repr(
# 66 "parsing/NANDparser.mly"
                                        ( Print(_3) )
# 282 "parsing/NANDparser.ml"
               : 'nandCom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 68 "parsing/NANDparser.mly"
                   ( _1 :: _3 )
# 290 "parsing/NANDparser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 69 "parsing/NANDparser.mly"
        ( [_1] )
# 297 "parsing/NANDparser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Binops.binop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 72 "parsing/NANDparser.mly"
                    ( Binop(_2, _1, _3) )
# 306 "parsing/NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : PL_functor.varID) in
    Obj.repr(
# 73 "parsing/NANDparser.mly"
            ( (*  (checkReadId $1); *)  Var(_1) )
# 313 "parsing/NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parsing/NANDparser.mly"
           ( Const(_1) )
# 320 "parsing/NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : PL_functor.funcID) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exps) in
    Obj.repr(
# 76 "parsing/NANDparser.mly"
       ( FxnApp(_1, _3) )
# 328 "parsing/NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 77 "parsing/NANDparser.mly"
                                ( _2 )
# 335 "parsing/NANDparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : PL_functor.varID) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ids) in
    Obj.repr(
# 79 "parsing/NANDparser.mly"
                     ((*(checkWriteId $1);*) _1 :: _3 )
# 343 "parsing/NANDparser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : PL_functor.varID) in
    Obj.repr(
# 80 "parsing/NANDparser.mly"
           ((* checkWriteId $1 ;*)  [_1] )
# 350 "parsing/NANDparser.ml"
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
