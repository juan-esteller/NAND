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

val parseProg :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> PL_functor.program
