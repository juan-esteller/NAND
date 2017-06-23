open PL_functor 

let parseStr (s: string): program  = 
  NANDparser.parseProg NANDlexer.token (Lexing.from_string s) 

(* utility function to generate a unique fresh variable, used throughout *) 
let freshVar : unit -> varID = 
  let cell = ref 0 in 
    fun () -> 
     let ind, _ = Int(!cell), (cell := !cell + 1) in
       ("unique", ind)  
(* utility function to generate a line of NAND code *) 
let genLine (id: varID) (l: string) (r: string)  : program = 
  parseStr ((strOfId id)^" := "^l^" NAND "^r^"\n")
 
let isUnary (e: exp) : bool = 
  match e with 
  | Var(_x) -> true 
  | Const(_x) -> true 
  | IsValid(_x) -> true
  | _ -> false  
(*
  Function to enable direct assignment (i.e. a := b)  
  ASSUMPTIONS: 
    - No function applications 
*) 
let enableAsg (c: command) : program = 
  match c with 
  | Asg([u], [e]) -> 
     if not (isUnary e) then 
      [c]
     else let newVar, eStr =  freshVar (), strOfExp e in 
       let newVarStr = strOfId newVar in 
         (genLine newVar eStr eStr) @ (genLine u newVarStr newVarStr) 
  | _ -> [c]

let enableAsg (p: program) : program = 
  List.concat (List.map enableAsg p) 
(* 
let constProg = 
  "notx_0 := x_0 NAND x_0
   one := notx_0 NAND x_0
   zero := one NAND one" 

(* assumes that program has already expanded all function applications *) 
let enableConst (p: program) : program = *) 
