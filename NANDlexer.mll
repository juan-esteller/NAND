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
}

let sym = ":=" | "NAND"
let ind = ['0' - '9']+ | 'i'  
let vID = ['a' - 'z']+  
rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* skip whitespace *)
  | "//" [^'\n']* '\n' { token lexbuf } (* skip one-line comments *)
  | sym as s { Hashtbl.find symTable s }
  | (vID as word)"_" (ind as i) 
      { let iStr = indOfString i in 
          match word with 
          | "isvalid" -> IS_VALID(iStr)  
          | _ -> ID((word, iStr))   }
  | (vID as word) {ID((word, Int(0))) } 
  | eof { EOF }
