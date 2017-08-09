open PL_functor 

let movingright = ("movingright", Int(0))
 
let incr (wasoriginal: varID) : exp = 
  FxnApp("AND", [Var(movingright); Var(wasoriginal)])

let decr (wasoriginal: varID) : exp = 
  FxnApp("AND", [FxnApp("NOT", [Var(movingright)]); Var(wasoriginal)])

let indexopList = 
    [("--", incr); 
     ("++", decr)]
