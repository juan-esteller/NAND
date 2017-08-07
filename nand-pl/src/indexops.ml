open PL_functor 

let movedright = ("movedright", Int(0))
 
let incr (wasoriginal: varID) : exp = 
  FxnApp("AND", [Var(movedright); Var(wasoriginal)])

let decr (wasoriginal: varID) : exp = 
  FxnApp("AND", [FxnApp("NOT", [Var(movedright)]); Var(wasoriginal)])

let indexopList = 
    [("--", incr); 
     ("++", decr)]
