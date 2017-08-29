open PL_functor
open PL_modules
open SS

(* assembles module for each PL using functor *) 
module NAND = PLFromBackEnd(NAND_back_end)  
module NAND_SS = SSFromBackEnd(NAND_back_end) 

module NANDPP = PLFromBackEnd(NANDPP_back_end)  
module NANDPP_SS = SSFromBackEnd(NAND_back_end) 

module NANDGG = PLFromBackEnd(NANDGG_back_end)  
module NANDGG_SS = SSFromBackEnd(NANDGG_back_end) 

(* record that provides interface for main.ml-- has addSS and execute methods-- 
   defaults to normal NAND, and also flag information *) 
type pl_interface = 
  { mutable execute: 
      PL_functor.program -> string -> string; 
    mutable addSS: 
      PL_functor.program -> PL_functor.program; 
   
    (* various flags *) 
    mutable ssSwitch: bool; 
    mutable isSilent: bool; 
    mutable dryRun: bool; 
  }

(* stores operations for whatever NAND language is in use *) 
let nand = 
  { execute = NAND.execute; 
    addSS = NAND_SS.addSS; 
    ssSwitch = false; 
    isSilent = false;
    dryRun = false; 
  } 

