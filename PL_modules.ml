open PL_functor 
open Binops 

(* module for backend of NAND *)
module NAND_back_end : PL_back_end =
  struct
    (* exception for repated gates *)
    exception Repeated_gate of string

    (* exception for indidces that aren't integral *)
    exception Invalid_index

    let evalIndex (st: store) (ind: index) : int = 
      match ind with 
      | Int(x) -> x 
      | _ -> raise Invalid_index

    let supportsBinop (b: binop) : bool = 
      b = "NAND" 

    (* always finishes after one iteration *)
    let supportsLoop = false 
    
    let supportsI = false
end

module NANDPP_back_end : PL_back_end =
  struct
    let evalIndex (st: store) (ind: index) : int =
      match ind with
      | I -> safeFind "i" st 
      | Int(x) -> x
     
    let supportsBinop (b: binop) : bool = 
      b = "NAND" 
 
    let supportsLoop = true 
    
    let supportsI = false 
end 


module NANDGG_back_end : PL_back_end = 
  struct 
    let evalIndex (st: store) (ind: index) : int = 
      match ind with 
      | I -> safeFind "i" st
      | Int(x) -> x 
    let supportsBinop (b: binop) : bool = 
      true 
    
    let supportsLoop = true 
    
    let supportsI = true
end
