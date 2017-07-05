type binop = string 

let nand (x: int) (y: int) : int = 
 if x * y > 0 then 
    0 
 else 
    1 

let binopList = 
 [("NAND", nand)]

let binopOfStr (b: string): int -> int -> int = 
  List.assoc b binopList   
