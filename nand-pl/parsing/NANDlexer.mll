{ 
  open PL_functor 
  open NANDparser ;;
  open Binops ;; 

  let symTable =
    Hashtbl.create 30

  let binops = List.map (fun (x, _y) -> (x, BINOP(x))) binopList 
  let oplist = binops 
 
  let _ = List.iter  (fun (kwd, tok) -> Hashtbl.add symTable kwd tok)
                       ([ (":=", ASG);
                          ("def", DEF);
                          (",", COMMA);  
                          ("(", LEFT_PAREN); 
                          (")", RIGHT_PAREN); 
                          ("{", LEFT_BRACK); 
                          ("}", RIGHT_BRACK);
                          ("if", IF);   
                          ("while", WHILE); 
                          ("print", PRINT)
                        ] @ oplist) 

  (* conversion from string to index *) 
  let indOfString (s : string) : index = 
    try 
      Int(int_of_string s)
    with Failure _s -> 
      I

  exception Invalid_variableID of string 
  
  let invalidIds = ["loop"; "i"; "j"; "k"; "l"; "m";] 

  let checkIdBody (body: string) : unit = 
   if List.mem body invalidIds then
     raise (Invalid_variableID(body))

}

let sym = ":=" | "," | "def" | "while" |  "(" | ")" | "{" | "}" | "if" | 
          "NAND" | "+" | "==" | "-" | "/" | "*" | "<" | ">" | "%" | "^" | "&&" | 
          "<<" | ">>"  | "print"  

let ind = ['0' - '9']+ | 'i'  
let vBod = ['a' - 'z']+  
let funcId = ['A' - 'Z']+

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* skip whitespace *)
  | "//" [^'\n']* '\n' { token lexbuf } (* skip one-line comments *)
  | sym as s { Hashtbl.find symTable s }
  | (vBod as word) ("\'"* as primes) "_" (ind as i) 
      { let iStr = indOfString i in
         let _ = checkIdBody word in  
          match word with 
          | "validx" -> if String.length primes = 0 then
                          VAR_ID(word, iStr) 
                        else 
                          raise (Invalid_variableID("validx"^primes))  
          | _ -> VAR_ID((word^primes, iStr))   }
  | (vBod as word) {VAR_ID((word, Int(0))) } (* parses loop as "loop_0", and in general adds index *) 
  | (funcId as word) { FUNC_ID(word) }  
  | eof { EOF }
