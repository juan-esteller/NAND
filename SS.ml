open PL_functor 

(* DECLARATIONS OF HELPER FUNCTIONS  *) 

(* parses a program as a string *) 
let parseStr (s: string): program  = 
  NANDparser.parseProg NANDlexer.token (Lexing.from_string s) 

(* utility function to generate a line of NAND code *) 
let genLine (id: varID) (l: string) (r: string)  : program = 
  parseStr ((strOfId id)^" := "^l^" NAND "^r^"\n")

(* utility function to generate a unique fresh variable, used throughout *) 
let freshVar : unit -> varID = 
  let cell = ref 0 in 
    fun () -> 
     let ind, _ = Int(!cell), (cell := !cell + 1) in
       ("unique", ind)  

(* determines whether an expression is simply a value *)  
let isValue (e: exp) : bool = 
  match e with 
  | Var(_x) -> true 
  | Const(_x) -> true 
  | IsValid(_x) -> true
  | _ -> false

(* maps a macro that maps a command to a sequence of commands to entire program *) 
let mapToProg (macro: command -> program) (p: program): program = 
  List.concat (List.map macro p)  

(* END OF HELPER FUNCTIONS *) 

(* START OF MACROS, in order of increasing complexity *) 

(* macro to expand list of assignments to lines *) 
let unzipCom (c: command) : program = 
  let pair (v: varID) (e: exp) : command = Asg([v], [e]) in 
    match c with 
    | Asg(vars, exps) -> 
       List.map2 pair vars exps 
    | _ -> [c]

let unzipProg (p: program) : program = 
  mapToProg unzipCom p

(* enables direct assignment (i.e. a := b)  *) 
let enableAsgCom (c: command) : program = 
  match c with 
  | Asg([u], [e]) -> 
     if not (isValue e) then 
      [c]
     else let newVar, eStr =  freshVar (), strOfExp e in 
       let newVarStr = strOfId newVar in 
         (genLine newVar eStr eStr) @ (genLine u newVarStr newVarStr) 
  | _ -> [c]

let enableAsgProg (p: program) : program = 
  mapToProg enableAsgCom p
(* 
let constProg = 
  "notx_0 := x_0 NAND x_0
   one := notx_0 NAND x_0
   zero := one NAND one" 

(* assumes that program has already expanded all function applications *) 
let enableConst (p: program) : program = *) 
