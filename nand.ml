(* type for bits *)
type bit = One | Zero

(* type for variables *)
type varID = string

  (* type for commands *)
  type com = Nand of varID * varID * varID

  (* type for a program *)
  type prog = com list

  (* module for maps from variables *)
  module VarMap = Map.Make(String)

  type store = bit VarMap.t

  (* straightforward NAND function *)
  let nand (l: bit) (r: bit) =
    match l, r with
    | One, One -> Zero
    | _ -> One

  (* exception for usage of an invalid character *)
  exception Invalid_char of char


  let stringOfBit (b: bit) : string =
    match b with
    | Zero -> "0"
    | One -> "1"

  

  (* returns line representing command*)
  let stringOfCom (c: com) =
    let Nand(x, y, z) = c in
      x^" := "^y^" NAND "^z

  (* returns string representing a program*)
  let stringOfProg (p: prog) : string =
    (String.concat "\n" (List.map stringOfCom p))^"\n"


  (* tries to extract value of variable; if not found, returns Zero, as per
     cs121notes.pdf pg. 43 *)
  let extractVal (v: varID) (s: store) : bit =
    try VarMap.find v s with Not_found -> Zero

  let printCom (c: com) (s: store) (i: int) : unit =
    let Nand(x, y, z) = c in
    let yV, zV = extractVal y s, extractVal z s in
    let xV = nand yV zV in
    let line = stringOfCom c in
      Printf.printf "Executing step %i: \"%s\", %s = %s, %s = %s, %s is assigned %s,\n"
                i line y (stringOfBit yV) z (stringOfBit zV) x (stringOfBit xV)

  exception Bound_variable of varID

  (* evaluates a program by returning final store *)
  let evalProg (p: prog) (xVals: string) (printVals: bool) : store =
    let rec helpEval (p: prog) (s: store) (i: int) =
      match p with
      | [] -> s
      | h::t ->
        let Nand(x, y, z) = h in
          if VarMap.mem x s then
            raise (Bound_variable x)
          else
            let _ = (if printVals then printCom h s i) in
            let yVal, zVal = extractVal y s, extractVal z s in
            let newVal = nand yVal zVal in
              helpEval t (VarMap.add x newVal s) (i + 1)
   in helpEval p (storeOfString xVals) 1

  

  (* given a program and string input, returns string output *)
  let evaluate (p: prog) (xVals: string) (print: bool) : string =
    evalStore (evalProg p xVals print)

