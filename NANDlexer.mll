{
  open NANDparser
  let symTable =
    Hashtbl.create 30

  let _ = List.iter  (fun (kwd, tok) -> Hashtbl.add symTable kwd tok)
                        [ (":=", ASG);
                          ("NAND", NAND);
                          ("(", L_PAREN);
                          (")", R_PAREN);
                          ("{", L_BRACKET);
                          ("}", R_BRACKET);
                          (",", COMMA);
                          ("def", DEF);
                          ("->", TO);
                          (":", COLON);
                        ]
}

let vID = ['a' - 'z']+'\''*(_['0'-'9']+)?
let sym = ":=" | "NAND" | "(" | ")" | "{" | "}" | "," | "->" | ":"
let const = "0" | "1"
let fID = ['A' - 'Z']+
let macroID = "@"['A' - 'Z']+
let plType = ['A' - 'Z']['a' - 'z']*
let macroEnd = macroID"_END"

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* skip whitespace *)
  | "//" [^'\n']* '\n' { token lexbuf } (* skip one-line comments *)
  | sym as s { (Printf.printf "found symbol %s\n" s); Hashtbl.find symTable s  }
  | vID as word { ID (word) }
  | fID as id { FUNC_ID(id) }
  | const as c
    { match c with
      | '0' -> CONST(false)
      | '1' -> CONST(true) }
  | macroEnd as m  { let macroName = String.sub m 1 ((String.length m) - 5) in
                        (Printf.printf "found macro end %s\n" macroName); MACRO_END(macroName) }
  | macroID as m { let macroName = String.sub m 1 ((String.length m) - 1) in
                    (Printf.printf "found macro %s\n" macroName); MACRO_START(macroName) }
  | plType as p { (Printf.printf "found pl type %s\n" p); PL_TYPE(p) }
  | eof { EOF }
