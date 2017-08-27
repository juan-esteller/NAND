open PL_data 
open Parse_flags

let () =
  let progIndex = parseFlags () in 
    if (Array.length Sys.argv) - progIndex <> 2 then
     ((Format.printf "Usage: ./main.native [flags] <file> <binary input>\n"); exit 0)
  else  
    let output = File_exec.executeFile Sys.argv.(progIndex) Sys.argv.(progIndex + 1) in
      Printf.printf "Output is %s\n" output
