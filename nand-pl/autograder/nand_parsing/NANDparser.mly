%{
open PL_functor;;
open Binops ;; 

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
%}

/* Declaration of tokens */
%token                                 EOF
%token <PL_functor.varID>              VAR_ID
%token <Binops.binop>                  BINOP 
%token                                 ASG
%token                                 COMMA
%token <int>                           CONST
%token                                 LEFT_PAREN RIGHT_PAREN
%token                                 LEFT_BRACK RIGHT_BRACK
%token                                 DEF
%token                                 WHILE 
%token <PL_functor.funcID>             FUNC_ID
%token                                 IF 
%token                                 PRINT 

/* Declarations of associativity */
%left BINOP  
 
%start parseProg
%type <PL_functor.program> parseProg  

%%
parseProg: nandProg EOF { $1 }

nandProg: nandCom nandProg {$1 :: $2}
     | nandCom {[$1]}

nandCom:
  | ids ASG exps {Asg($1, $3)}
  | DEF ids ASG FUNC_ID LEFT_PAREN ids RIGHT_PAREN LEFT_BRACK nandProg RIGHT_BRACK  
       { FxnDef(($4, {inputs = $6; outputs = $2; body = $9 })) }
  | IF LEFT_PAREN exp RIGHT_PAREN LEFT_BRACK nandProg RIGHT_BRACK 
       { If($3, $6) } 
  | WHILE LEFT_PAREN exp RIGHT_PAREN LEFT_BRACK nandProg RIGHT_BRACK
       { While($3, $6) } 
  | PRINT LEFT_PAREN VAR_ID RIGHT_PAREN { Print($3) }  
exps:
  | exp COMMA exps { $1 :: $3 }  
  | exp { [$1] }  

exp: 
   | exp BINOP  exp { Binop($2, $1, $3) } 
   | VAR_ID { (*  (checkReadId $1); *)  Var($1) } 
   | CONST { Const($1) } 
   | FUNC_ID LEFT_PAREN exps RIGHT_PAREN
       { FxnApp($1, $3) } 
   | LEFT_PAREN exp RIGHT_PAREN { $2 }  
ids:
  | VAR_ID COMMA ids {(*(checkWriteId $1);*) $1 :: $3 } 
  | VAR_ID {(* checkWriteId $1 ;*)  [$1] }  
   
;
