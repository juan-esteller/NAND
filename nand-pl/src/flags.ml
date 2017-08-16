open PL_data

let processFlag (str: string) : unit = () 

(* source file for parsing run-time flags *) 
let isFlag (str: string) : bool = 
  str.[0] = '-' 

(* current argument that is being parsed *) 
let curArg = 
  ref 0 

let checkIsFlag (() : unit) : bool =  
  let _ = (curArg := !curArg + 1) in 
   isFlag Sys.argv.(!curArg) 
  
(* parses all flags, and returns index where program and input start *) 
let parseFlags (() : unit) : int = 
  let _ = 
    while checkIsFlag () do 
     (processFlag Sys.argv.(!curArg)); 
    done  
  in 
    !curArg  


