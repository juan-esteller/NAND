open PL_data 
open Parse_flags

let () =
  let progIndex = parseFlags () in 
    if (Array.length Sys.argv) - progIndex <> 2 then
     ((Format.printf "Usage: ./main.native [flags] <file> <binary input>\n"); exit 0)
  else  
    let output = Prog_exec.executeFile Sys.argv.(progIndex) Sys.argv.(progIndex + 1) in
      if not nand.dryRun then 
        Printf.printf "Output is %s\n" output
