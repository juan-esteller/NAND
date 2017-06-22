%{
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
   | ID { (checkReadId $1); Var($1) }
   | IS_VALID { IsValid($1) }
 
ids: 
  | ID {(checkWriteId $1);  [$1] }   
;
