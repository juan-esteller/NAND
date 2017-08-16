open PL_data 
open Flags

let () =
  let progIndex = parseFlags () in 
    if (Array.length Sys.argv) - progIndex <> 2 then
     ((Format.printf "Usage: nand <flags> <file> <binary input>\n"); exit 0)
  else  
    let output = File_exec.executeFile nand.execute nand.addSS Sys.argv.(progIndex) Sys.argv.(progIndex + 1) in
      Printf.printf "Output is %s\n" output
