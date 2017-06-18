{
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
      Int(int_of_string i)
    with Failure _s -> 
      I
  let expOfBase (b: string) (i: string): exp = 
    let ind = indOfString i in 
    match b with 
    | "validx" -> IsValid(ind) 
    | _s -> Var((b, ind))    
      
}

let sym = ":=" | "NAND"
let ind = ['0' - '9']+ | 'i'  
let base = ['a' - 'z']+  
rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* skip whitespace *)
  | "//" [^'\n']* '\n' { token lexbuf } (* skip one-line comments *)
  | sym as s { Hashtbl.find symTable s }
  | (base)(ind as i) {
      
    }     
  | (vID as word) (index as i)  { ID (word) }
  | eof { EOF }
