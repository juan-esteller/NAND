(* open progType.ml ;; *) (*uncomment to make *)


(* exception for invalid commands *) 
exception Invalid_command

(* simple NAND function *)
let nand (l: bit) (r: bit) : bit = 
  match l, r with 
  | One, One -> Zero
  | _ -> One 

let bitOfBool (b: bool) : bit = 
  match b with 
  | true -> One
  | false -> Zero

(* module for backend of NAND *) 
module Nand_back_end : PL_back_end = 
  struct 
    (* exception for repated gates *) 
    exception Repeated_gate of varID 
    
    (* exception for indidces that aren't integral *) 
    exception Invalid_index 

    let assertInt (v: varID) : unit =
      match v with
      | (_str, Int(_x)) -> () 
      | _notValid -> raise Invalid_index   
   
    (* straightforward evaluation of a line; ignores pData *) 
    let evalCom (c: command) (st: store ref) (_pData: progData ref) : unit =  
      match c with 
      | Asg([h], [Nand(Var(u), Var(v))]) -> 
         (* checks that indices are valid *) 
         let _ = begin assertInt h; assertInt u; assertInt v; end in 
         let hStr = strOfId h in 
         if VarMap.mem hStr !st then 
           raise (Repeated_gate(h)) 
         else let newValue = nand (safeFind u !st) (safeFind v !st) in 
               (st := VarMap.add hStr newValue !st)   
      | _ -> raise Invalid_command 
    
    (* always finishes after one iteration *)
    let endCondition (_st: store) : bool = true 
end 

module NandPP_back_end : PL_back_end = 
  struct
    let evalIndex (pData: progData) (ind: index) : int = 
      match ind with 
      | I -> pData.i 
      | Int(x) -> x 

    (* makes variable have a numerical index *) 
    let extractId (pData: progData) (id: varID) : varID = 
      let (s, ind) = id in 
        (s, Int(evalIndex pData ind)) 
  
    (* utility function to evaluate an expression *) 
    let evalExp (e: exp) (st: store) (pData: progData) : bit = 
      match e with 
      | Var(u) -> safeFind (extractId pData u) st 
      | IsValid(i) -> bitOfBool ((evalIndex pData i) < pData.n)
      | _ -> raise Invalid_command
 
    (* straightforward evaluation of a command *)           
    let evalCom (c: command) (st: store ref) (pData: progData ref) : unit = 
      match c with
      | Asg([h], [Nand(e1, e2)]) -> 
         let l, r = evalExp e1 !st !pData, evalExp e2 !st !pData in
           (st := (VarMap.add (strOfId (extractId !pData h))  (nand  l r) !st))
      | _ -> raise Invalid_command

   let endCondition (st: store): bool  = 
     (safeFind ("loop", Int(0)) st) = Zero (* ends program in case loop set to Zero *) 
end 
