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

(* converts a char to a bit *)
let bitOfChar (c: char) : bit =
  match c with
  | '0' -> Zero
  | '1' -> One
  | _invalid -> raise (Invalid_char c)

let stringOfBit (b: bit) : string =
  match b with
  | Zero -> "0"
  | One -> "1"

(* creates initial store provided binary string *)
let storeOfString (xVals: string) : store =
  let s = ref VarMap.empty in
  let add (i: int) (c: char) : unit =
    let id = "x_"^(string_of_int i) in
      s := VarMap.add id (bitOfChar c) !s
  in (String.iteri add xVals); !s


(* tries to extract value of variable; if not found, returns Zero, as per
   cs121notes.pdf pg. 43 *)
let extractVal (v: varID) (s: store) : bit =
  try VarMap.find v s with Not_found -> Zero

exception Bound_variable of varID

(* evaluates a program by returning final store *)
let evalProg (p: prog) (xVals: string) : store =
  let rec helpEval (p: prog) (s: store) =
    match p with
    | [] -> s
    | h::t -> let Nand(x, y, z) = h in
                if VarMap.mem x s then
                  raise (Bound_variable x)
                else let newVal = nand (extractVal y s) (extractVal z s) in
                  helpEval t (VarMap.add x newVal s)
 in helpEval p (storeOfString xVals)

(* returns output of a store as a binary string *)
let evalStore (s: store) : string =
  let rec helpEvalStore (s: store) (i: int) : string =
    let id = "y_"^(string_of_int i) in
      if VarMap.mem id s then
        (stringOfBit (VarMap.find id s))^(helpEvalStore s (i + 1))
      else
        ""
in helpEvalStore s 0

(* returns string representing a program*)
 let stringOfProg (p: prog) : string =
   let stringOfCom (c: com) =
     let Nand(x, y, z) = c in
       x^" := "^y^" NAND "^z
   in (String.concat "\n" (List.map stringOfCom p))^"\n"

let evaluate (p: prog) (xVals: string) : string =
  evalStore (evalProg p xVals)
