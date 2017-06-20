%{
open PL_functor;; 
%}

/* Declaration of tokens */
%token                                 EOF
%token <PL_functor.varID>              ID
%token                                 NAND
%token                                 ASG
%token <PL_functor.index>              IS_VALID 
%token                                 COMMA

/* Declarations of associativity */
%left NAND
 
%start parseProg
%type <PL_functor.program> parseProg  

%%
parseProg: nandProg EOF { $1 }

nandProg: nandCom nandProg {$1 :: $2}
     | nandCom {[$1]}

nandCom:
  | ids ASG exps {Asg($1, $3)}

exps:
  | exp COMMA exps { $1 :: $3 }  
  | exp { [$1] }  

exp: 
   | exp NAND exp { Nand($1, $3) } 
   | ID { Var($1) }
   | IS_VALID { IsValid($1) }
 
ids: 
  | ID { [$1] }   
;
