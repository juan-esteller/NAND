open PL_functor 
open PL_modules
open File_exec
open SS 

module NAND = PLFromBackEnd(NANDPP_back_end)  
module SS_lang = SSFromBackEnd(NANDPP_back_end) 
let () =
  let _ =
    if Array.length Sys.argv <> 3 then
     ((Format.printf "Usage: nand <file> <flags> <binary input>\n"); exit 0) in
  let output = File_exec.executeFile NAND.execute SS_lang.addSS Sys.argv.(1) Sys.argv.(2) in
      Printf.printf "Output is %s\n" output
