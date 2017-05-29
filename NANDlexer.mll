{
  open NANDparser
}

let asg = ":="
let nand = "NAND"
let id = ['a' - 'z']+(_['0'-'9']+)?'\''*

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* skip whitespace *)
  | "//" [^'\n']* '\n' { token lexbuf } (* skip one-line comments *)
  | id as word { ID (word) }
  | asg { ASG }
  | nand { NAND }
  | eof { EOF }
