  open PL_functor 

(* simple NAND function *)
let nand (l: bit) (r: bit) : bit =
  match l, r with
  | One, One -> Zero
  | _ -> One

let bitOfBool (b: bool) : bit =
  match b with
  | true -> One
  | false -> Zero

(* module for backend of NAND *)
module NAND_back_end : PL_back_end =
  struct
    (* exception for repated gates *)
    exception Repeated_gate of string

    (* exception for indidces that aren't integral *)
    exception Invalid_index

    let assertValid (v: varID) : unit =
      match v with
      | (body, Int(_x)) -> 
        (if body = "loop" then
           raise Invalid_index)  
      | _notValid -> raise Invalid_index

    (* straightforward evaluation of a line; ignores pData *)
    let evalCom (st: store ref) (_pData: progData) (c: command) : comVals =  
     let f (h: varID) (l: exp) (r: exp) : comVals =
        match l, r with
        | Var(u), Var(v) ->
          let _ = begin assertValid h; assertValid u; assertValid v; end in
           let hStr = strOfId h in
             let lhsVal, rhsVal = varFind u !st, varFind v !st in
               let newValue = nand lhsVal rhsVal in
               { result = hStr; resultVal = newValue; 
                 lhs = strOfId u; lhsVal = lhsVal; 
                 rhs = strOfId v; rhsVal = rhsVal; 
               } 
        | _ -> raise Invalid_command
     in mapOverCom f c

    (* always finishes after one iteration *)
    let endCondition (_st: store) : bool = true
end

module NANDPP_back_end : PL_back_end =
  struct
    let evalIndex (pData: progData) (ind: index) : int =
      match ind with
      | I -> pData.i
      | Int(x) -> x

    (* makes variable have a numerical index *)
    let extractId (pData: progData) (id: varID) : varID =
      let (s, ind) = id in
        (s, Int(evalIndex pData ind))

    (* utility function to evaluate an expression *)
    let evalExp (e: exp) (st: store) (pData: progData) : bit =
      match e with
      | Var(u) -> varFind (extractId pData u) st
      | IsValid(i) -> bitOfBool ((evalIndex pData i) < pData.n)
      | _ -> raise Invalid_command

    (* straightforward evaluation of a command *)
    let evalCom (st: store ref) (pData: progData) (c: command) : comVals =
      let f (h: varID) (l: exp) (r: exp) : comVals =
        let u, v = evalExp l !st pData, evalExp r !st pData in
          let id = extractId pData h in 
            let idStr, newValue  = strOfId id, nand u v in
              { result = strOfId id; resultVal = newValue; 
                lhs = strOfExp l; lhsVal = u; 
                rhs = strOfExp r; rhsVal = v;
              }   
    in mapOverCom f c

   let endCondition (st: store): bool  =
     (safeFind "loop" st) = Zero (* ends program in case loop set to Zero *)
end
