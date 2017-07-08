open PL_functor 

(* module for backend of NAND *)
module NAND_back_end : PL_back_end =
  struct
    (* exception for repated gates *)
    exception Repeated_gate of string

    (* exception for indidces that aren't integral *)
    exception Invalid_index

    let evalIndex (pData: progData) (ind: index) : int = 
      match ind with 
      | Int(x) -> x 
      | _ -> raise Invalid_index
 
    (* always finishes after one iteration *)
    let supportsLoop = false 
end

module NANDPP_back_end : PL_back_end =
  struct
    let evalIndex (pData: progData) (ind: index) : int =
      match ind with
      | I -> pData.i
      | Int(x) -> x
    
    let supportsLoop = true 
end
