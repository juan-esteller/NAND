%{
    open PL_functor 
    open PL_data 

    (* program that we'll be checking *) 
    let checkProg = 
     try 
       Prog_exec.parseFile Sys.argv.(1)  
     with 
     | _ -> 
      let _ = Printf.fprintf stderr "ERROR: Failed to parse input .nand file\n" in 
      let _ = Printf.printf "0, 0.0\n" in
        exit 1 

    let line = ref 0
    let wrong = ref 0
%}

/* Declaration of tokens */
%token             COMMA 
%token <string>    STRING 
%token             EOF

 
%start parseCSV
%type <unit> parseCSV  

%%
parseCSV: parseLines EOF { $1 }

parseLines: 
  | lastline 
    { let _ =  $1 in 
      let f_wrong, f_line = float_of_int !wrong, float_of_int !line in 
      let pct = ((f_line -. f_wrong) /. f_line) in 
        Printf.printf "%i, %f\n" !line pct 
    } 
  | line parseLines { begin $1; $2; end} 

line: 
  | STRING COMMA STRING 
    {
      let _ = (line := !line + 1) in  
      let input = $1 in 
      let output = $3 in 
      let _ = Printf.printf "Giving %s as input to program\n" input in 
      let prog_output = PL_data.nand.execute checkProg input  in 
        if prog_output <> output then
          begin  
            (Printf.fprintf stderr "ERROR: Line %i: Input %s, expected %s, actual %s\n" 
             !line input output prog_output); 
             (wrong := !wrong + 1);  
           end
    } 

lastline: 
  | line { $1 }  
;
