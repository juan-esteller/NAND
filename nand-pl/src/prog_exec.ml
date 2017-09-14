open PL_functor 
open PL_modules
open PL_data 
open SS 


let executeProgram (program: PL_functor.program) (input: string) : string =
   let program = 
    if nand.ssSwitch then 
      nand.addSS program
    else 
      program 
   in if nand.dryRun then 
       let _ =  Printf.printf "%s\n" (strOfProg program)  in 
         ""  
      else 
        nand.execute program input 

let parseChannel (inputToBuf: 'a -> Lexing.lexbuf) 
  (prog: 'a) : PL_functor.program =
  let lexbuf = inputToBuf prog in
      NANDparser.parseProg NANDlexer.token lexbuf

let executeChannel (inputToBuf: 'a -> Lexing.lexbuf) (prog: 'a) (input: string) : string = 
  executeProgram (parseChannel inputToBuf prog) input  

let executeFile (filename: string) (input: string) : string = 
  let startExt = (String.rindex filename '.') + 1 in
  let ext = String.sub filename startExt ((String.length filename) - startExt) in
    let _ =
      if ext <> "nand" then
       ((Format.printf "Must provide a .nand file as input\n"); exit 0) 
    in executeChannel Lexing.from_channel  (open_in filename) input 

let executeString (prog: string) (input: string) : string = 
  executeChannel Lexing.from_string prog input  
   
