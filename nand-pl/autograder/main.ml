open PL_data 
open Parse_flags

let () =
   let csv = Sys.argv.(2) in 
   let _ = Flags.makeSilent () in 
     Parser.parseCSV Lexer.token (Lexing.from_channel (open_in csv)) 
