{ 
  open PL_functor 
  open NANDparser
  let symTable =
    Hashtbl.create 30

  let _ = List.iter  (fun (kwd, tok) -> Hashtbl.add symTable kwd tok)
                        [ (":=", ASG);
                          ("NAND", NAND);
                        ]

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

let sym = ":=" | "NAND"
let ind = ['0' - '9']+ | 'i'  
let vBod = ['a' - 'z']+  
rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* skip whitespace *)
  | "//" [^'\n']* '\n' { token lexbuf } (* skip one-line comments *)
  | sym as s { Hashtbl.find symTable s }
  | (vBod as word) ("\'"* as primes) "_" (ind as i) 
      { let iStr = indOfString i in
         let _ = checkIdBody word in  
          match word with 
          | "validx" -> if String.length primes = 0 then
                          IS_VALID(iStr) 
                        else 
                          raise (Invalid_variableID("validx"^primes))  
          | _ -> ID((word^primes, iStr))   }
  | (vBod as word) {ID((word, Int(0))) } (* parses loop as "loop_0", and in general adds index *)  
  | eof { EOF }
