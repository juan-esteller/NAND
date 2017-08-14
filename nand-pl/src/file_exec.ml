open PL_functor 
open PL_modules
open SS 

let executeFile (execute: program -> string -> string) (addSS: program -> program) 
                (fileName: string) (input: string): string  =
  let startExt = (String.rindex fileName '.') + 1 in
  let ext = String.sub fileName startExt ((String.length fileName) - startExt) in
    let _ =
      if ext <> "nand" then
       ((Format.printf "Must provide a .nand file as input\n"); exit 0) in
  let file = open_in fileName in
  let lexbuf = Lexing.from_channel file in
  let program = NANDparser.parseProg NANDlexer.token lexbuf in
  let program = addSS program in  
     execute program input 
