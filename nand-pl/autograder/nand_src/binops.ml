type binop = string 

let makeReturnBit (f: int -> int -> bool) (l: int) (r: int) : int = 
  if f l r then 1 else 0 

let nand (x: int) (y: int) : int = 
 if x * y > 0 then 
    0 
 else 
    1 

let binopList = 
 [("NAND", nand); 
  ("+", (+)); 
  ("-", (-));
  ("/", (/)); 
  ("==", makeReturnBit (=)); 
  ("*", ( * )); 
  ("<", makeReturnBit (<)); 
  (">", makeReturnBit (>));
  ("%", ( mod ));
  ("&&", ( land )); 
  ("^",  ( lxor )); 
  (">>", ( lsr )); 
  ("<<", ( lsl ));]

let binopOfStr (b: string): int -> int -> int = 
  List.assoc b binopList   
