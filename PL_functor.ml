open Binops ;; 

(* indices into variables *)
type index =
 | Int of int
 | I

(* converts indices into strings *)
let strOfIndex (i: index) : string =
  match i with
  | Int(x) ->"_"^(string_of_int x)
  | I -> "_"^"i"

(* Type for variable ID's; first string corresponds to
   the base, second string corresponds to the index--
   in the case that it corresponds to an index, then
   second string will be empty  *)
type varID = string * index

let strOfId (id: varID) : string =
  let (body, ind) = id in
    if body = "loop" then
      "loop"
    else
      body^(strOfIndex ind)

(* type for arguments + outputs of functions *)
type args = varID list

(* type for function ID's *)
type funcID = string

type program =
  (* code is just a list of commands *)
  command list
and  command =
  (* assigns value of ith expression to ith varID;
     assumes that each expression has one output  *)
  | Asg of args * (exp list)
  (* if statement w/ intuitive meaning; also
     assumes expressions have one output *)
  | If of exp * program
  (* function definition; first varID list corresponds
     to output, varID list corresponds to input *)
  | FxnDef of funcID * func
and exp =
  | Const of int 
  | Var of varID
  | Binop of binop * exp * exp 
  | FxnApp of funcID * (exp list) (* expressions in list must be unary *)
and func = {
    inputs: args;
    outputs: args;
    body: program;
  }

exception Invalid_command
exception Invalid_expression

(* utility function to apply mapping to valid run-time commands *)
let mapOverCom (f: binop -> varID -> exp -> exp -> 'a) (c: command): 'a =
  match c with
  | Asg([h], [Binop(b, l, r)]) -> f b h l r
  | _ -> raise Invalid_command

(* converts valid run-time expressions to strings *)
let strOfExp (e: exp) : string =
  match e with
  | Var(x) -> strOfId x
  | Const(b) -> if b = 0  then "zero" else "one"
  | _ -> raise Invalid_command

(* ditto, for commands *)
let strOfCom: command ->  string =
  let f (b: binop) (h: varID) (l: exp) (r: exp) : string =
    (strOfId h)^" := "^(strOfExp l)^" "^b^" "^(strOfExp r)
  in mapOverCom f

(* function for converting valid run-time programs into strings *)

let strOfProg (p: program) : string =
  String.concat "\n" (List.map strOfCom p)

(* module for mapping varIDs (= Strings) to their bit values *)
module VarMap = Map.Make(String)

(* type for store of varID's values *)
type store = int VarMap.t

let safeFind (str: string) (st: store) : int =
  try VarMap.find str st with Not_found -> 0 

(* tries to find id in store, returns 0 if not found *)
let varFind (id: varID) (st: store) : int =
  safeFind (strOfId id) st


(* record for keeping track of various pieces of data*)
type progData =
  { mutable pc : int; (* program counter*)
    mutable r: int;   (* number of rounds *)
    mutable m: int;   (* maximum y index *)
    mutable i: int;   (* index that iterates up and down *)
    mutable inc: int;  (* increment for i *)
    n: int        (* length of x input *)
    }

module type PL_back_end = sig
  (* function that updates store & program data, raises
     an exception in the case that they are not supported, returns
     varID & its new value  *)
  val evalIndex : progData -> index -> int
  val supportsBinop : binop -> bool 
  val supportsLoop : bool
end




module type PL_type = sig
  val execute: program -> string -> string
end

exception Invalid_char of char
exception Invalid_bit of int
exception Invalid_binop of binop 
 
module PLFromBackEnd (Lang : PL_back_end) : PL_type =
  struct

    (* converts a char to a bit *)
    let bitOfChar (c: char) : int =
      match c with
      | '0' -> 0 
      | '1' -> 1 
      | _invalid -> raise (Invalid_char c)

    let stringOfBit (c: int) : string =
      match c with
      | 0  -> "0"
      | 1 -> "1" 
      | _ -> raise (Invalid_bit(c)) 

    (* creates initial store provided binary string *)
    let storeOfString (xVals: string) : store =
      let s = ref VarMap.empty in
        let add (i: int) (c: char) : unit =
          let id = "x_"^(string_of_int i) in
            s := VarMap.add id (bitOfChar c) !s
    in (String.iteri add xVals); !s

      (* returns output of a store as a binary string *)
    let stringOfStore (s: store) (m: int) : string =
      let rec helpEvalStore (s: store) (i: int) : string =
        let id = ("y", Int(i)) in
          if i < m then
            (stringOfBit (varFind id s))^(helpEvalStore s (i + 1))
          else
            ""
    in helpEvalStore s 0

    (* helper function that updates pData at end of an iteration *)
    let updateProgData (pData: progData) : unit =
       begin
          (if pData.i = 0 then
            begin (pData.r <- pData.r + 1); (pData.inc <- 1) end);
          (if pData.i = pData.r then
            (pData.inc <- -1));
          (pData.i <- pData.i + pData.inc);
          (pData.pc <- pData.pc + 1);
        end

    (* increments m if necessary *)
    let incM (pData: progData) (c: command) : unit =
      let extractIndexVal (id : varID) : int =
        let (body, ind) = id  in
          match body with
          | "y" -> (match ind  with
                    | I -> pData.i
                    | Int(x) -> x + 1)
          | _noty -> -1 (* won't cause any increase in m *)
    in match c with
        (* all commands that update m will be of this form
            after removal of SS *)
       | Asg([h], _exp) ->
           (pData.m <- max pData.m (extractIndexVal h))
       | _ignore -> ()
    
    let extractId (pData: progData) (x: varID) : string =
      let (body, ind) = x in
        if body = "loop" then
          if Lang.supportsLoop then
            "loop"
          else
            raise Invalid_expression
        else
          body^"_"^(string_of_int (Lang.evalIndex pData ind))

    let bitOfBool (b: bool) : int = 
      if b then 1 else 0 
   
    let evalExp (pData: progData) (st: store ref) (e: exp) : string * int =
      match e with
      | Var(x) ->
        let body, ind = x in 
          if body = "validx" then
            if not Lang.supportsLoop then 
              raise Invalid_expression 
            else 
              let i = Lang.evalIndex pData ind in 
                "validx"^"_"^(string_of_int i), bitOfBool (i < pData.n) 
           else let id = extractId pData x in
             id, safeFind id !st
      | _ -> raise Invalid_expression

    (* executes a command by updating store using Lang's function
       and incrementing m as necessary *)
    let execCommand (pData: progData) (st: store ref) (c: command) : unit =
      let eval = evalExp pData st in
      match c with
      | Asg([id], [Binop(b, l, r)]) ->
          if not (Lang.supportsBinop b) then 
             raise (Invalid_binop(b)) 
          else 
            let comStr = strOfCom c in 
              let (lhsId, lhsVal), (rhsId, rhsVal) = eval l , eval r in
                let binop = binopOfStr b in 
                  let resVal, resId = binop lhsVal rhsVal, extractId pData id in
        begin
         (st := VarMap.add resId resVal !st);
         (Printf.printf "Executing commmand \"%s\", %s has value %s, %s has value %s, %s assigned value %s\n"
                       comStr lhsId (stringOfBit lhsVal)
                              rhsId (stringOfBit rhsVal)
                              resId (stringOfBit resVal));
         incM pData c;
        end
      | _ -> raise Invalid_command

    (* evaluation of a program; will attempt to evaluate according
       to specification of Lang, raise an error in the case that
       there are any unsupported features in the AST *)
    let execute (p : program) (s: string) : string =
      (* initializes program data *)
     let pData =
          { pc = 0; r = 0; m = 0; i = 0; inc = 1; n = String.length s }
         (* initializes store *)
      in let st = ref (storeOfString s)
      (* loop that executes program until end condition is satisfied *)
      in let rec evalLoop ((): unit) : unit =
           begin
             List.iter (execCommand pData st) p;
             updateProgData pData;
             (*TODO: we'll eventually need to implement a time-out *)
              (if  Lang.supportsLoop && ((safeFind "loop" !st) = 1) then 
                  evalLoop ());  
           end
      in let _ = evalLoop () in
           stringOfStore !st pData.m
end
