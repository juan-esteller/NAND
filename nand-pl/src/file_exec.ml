open PL_functor 
open PL_modules
open PL_data 
open SS 

let executeFile (fileName: string) (input: string): string  =
  let startExt = (String.rindex fileName '.') + 1 in
  let ext = String.sub fileName startExt ((String.length fileName) - startExt) in
    let _ =
      if ext <> "nand" then
       ((Format.printf "Must provide a .nand file as input\n"); exit 0) in
  let file = open_in fileName in
  let lexbuf = Lexing.from_channel file in
  let program = NANDparser.parseProg NANDlexer.token lexbuf in
    if !ssSwitch then 
      nand.execute (nand.addSS program) input  
    else 
      nand.execute program input 
