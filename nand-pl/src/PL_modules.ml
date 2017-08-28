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
    
    let supportsAsg = false
    
    let muxDefStr = 
      "def a := MUX(b, c, d) { 
         nb := b NAND b
         u := c NAND nb 
         v := d NAND b 
         a := u NAND v 
       }" 
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
    
    let supportsI = true 
    
    let supportsAsg = false
    
    let muxDefStr = 
      "def a := MUX(b, c, d) { 
         nb := b NAND b
         u := c NAND nb 
         v := d NAND b 
         a := u NAND v 
       }" 
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
   
    let supportsAsg = true 
    
    let muxDefStr = 
       "def a := MUX(b, c, d) { 
         tmp_0 := c 
         tmp_1 := d
         temp_0 := i 
         i := b 
         res := tmp_i 
         i := temp_0 
         a := res  
       }"   
end
