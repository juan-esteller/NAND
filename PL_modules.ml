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
         v := d NAND c 
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
    
    let supportsI = false 
    
    let supportsAsg = false
    
    let muxDefStr = 
      "def a := MUX(b, c, d) { 
         nb := b NAND b
         u := c NAND nb 
         v := d NAND c 
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
         tmp_2 := i 
         i := b 
         a := tmp_i  
         i := tmp_2 
       }" 
end
