%{
open Nand
%}

/* Declaration of tokens */
%token EOF
%token <string> ID
%token NAND
%token ASG

%start input
%type <Nand.prog> input

%%

input: lines EOF { $1 }

lines: line lines {$1 :: $2}
     | line {[$1]}

line: ID ASG ID NAND ID { Nand($1, $3, $5) }
;
