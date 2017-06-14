(* open progType.ml ;; *) (*uncomment to make *)


(* exception for invalid commands *) 
exception Invalid_command

(* simple NAND function *)
let nand (l: bit) (r: bit) : bit = 
  match l, r with 
  | One, One -> Zero
  | _ -> One 

(* module for backend of NAND *) 
module Nand : PL_back_end = 
  struct 
    (* exception for repated gates *) 
    exception Repeated_gate of varID 
    
    (* straightforward evaluation of a line; ignores pData *) 
    let evalCom (c: command) (st: store ref) (_pData: progData ref) : unit =  
      match c with 
      | Asg([h], [Nand(Var(u), Var(v))]) -> 
         if VarMap.mem h !st then 
           raise (Repeated_gate(h)) 
         else let l, r = safeFind u !st, safeFind v !st in
           (st := VarMap.add h (nand l r) !st) 
      | _ -> raise Invalid_command 
    
    (* always finishes after one iteration *)
    let endCondition (_st: store) : bool = true 
end 
           
