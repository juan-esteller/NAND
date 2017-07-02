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

val parseProg :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> PL_functor.program
