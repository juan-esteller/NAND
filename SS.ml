open PL_functor 

(* DECLARATIONS OF HELPER FUNCTIONS  *) 

(* parses a program as a string *) 
let parseStr (s: string): program  = 
  NANDparser.parseProg NANDlexer.token (Lexing.from_string s) 

(* utility function to generate a line of NAND code *) 
let genLine (id: varID) (l: string) (r: string)  : program = 
  parseStr ((strOfId id)^" := "^l^" NAND "^r^"\n")

(* utility function to generate a unique fresh variable, used throughout *) 
let freshVar : unit -> varID = 
  let cell = ref 0 in 
    fun () -> 
     let ind, _ = Int(!cell), (cell := !cell + 1) in
       ("unique", ind)  

(* determines whether an expression is simply a value *)  
let isValue (e: exp) : bool = 
  match e with 
  | Var(_x) -> true 
  | Const(_x) -> true 
  | IsValid(_x) -> true
  | _ -> false

(* maps a macro that maps a command to a sequence of commands to entire program *) 
let mapToProg (macro: command -> program) (p: program): program = 
  List.concat (List.map macro p)  

(* END OF HELPER FUNCTIONS *) 

(* START OF MACROS, in order of increasing complexity *) 

(* macro to expand list of assignments to lines *) 
let unzipCom (c: command) : program = 
  let pair (v: varID) (e: exp) : command = Asg([v], [e]) in 
    match c with 
    (* leaves function applicatoins alone *) 
    | Asg(_vars, [FxnApp(_id, _args)]) -> [c] 
    (* otherwise assumes all expressions evaluate to a single value *) 
    | Asg(vars, exps) -> 
       List.map2 pair vars exps 
    | _ignore -> [c]

let unzipProg (p: program) : program = 
  mapToProg unzipCom p

(* enables direct assignment (i.e. a := b)  *) 
let enableAsgCom (c: command) : program = 
  match c with 
  | Asg([u], [e]) -> 
     if not (isValue e) then 
      [c]
     else let newVar, eStr =  freshVar (), strOfExp e in 
       let newVarStr = strOfId newVar in 
         (genLine newVar eStr eStr) @ (genLine u newVarStr newVarStr) 
  | _ -> [c]

let enableAsgProg (p: program) : program = 
  mapToProg enableAsgCom p

(* module for handling stores of functions *) 
module FuncMap = Map.Make(String) 

(* function store is a hashtable from function IDs to functions *) 
type funcStore =  func FuncMap.t 

let substProg (subsList: (varID * varID) list) (p: program) : program = 
  let substId (id: varID) : varID = 
    try 
     List.assoc id subsList
    with Not_found -> 
     let body, ind = id in 
       ("up"^body, ind) 
  in let substExp (e: exp) : exp = 
    match e with 
    | Var(id) -> Var(substId id) 
    | _ -> e
  in let substCom (c: command) : command = 
    match c with 
    | Asg([h], [Nand(e1, e2)]) -> Asg([substId h], [Nand(substExp e1, substExp e2)]) 
    | _ -> raise Invalid_command   
  in List.map substCom p
 
exception Invalid_input of exp 

let strip (e: exp) : varID = 
  match e with
  | Var(id) -> id 
  | _ -> raise (Invalid_input(e))
  
let expandFunc (ids: varID list) (args: exp list) (f: func): program = 
  let zip = List.map2 (fun x y -> (x, y)) in 
    let subsList = (zip f.outputs ids) @ (zip f.inputs (List.map strip args)) in 
      substProg subsList f.body   

exception Unbound_function of funcID 

(* processes a line in a program with function definitions *) 
let rec enableFuncCom (st: funcStore ref) (c: command) : program = 
  match c with 
  | FxnDef(fId, f) -> 
     let curStore = !st in 
       let newBody = mapToProg (enableFuncCom st) f.body in 
         let newFun = {f with body = newBody} in 
           (st := FuncMap.add fId newFun  curStore); [] 
  | Asg(ids, [FxnApp(fId, args)]) -> 
     (try 
       expandFunc ids args (FuncMap.find fId !st)  
      with Not_found -> raise (Unbound_function(fId))) 
  | _ -> [c]

let enableFuncProg (p: program) : program = 
  let st = ref FuncMap.empty in
    mapToProg (enableFuncCom st) p
(* 
let constProg = 
  "notx_0 := x_0 NAND x_0
   one := notx_0 NAND x_0
   zero := one NAND one" 

(* assumes that program has already expanded all function applications *) 
let enableConst (p: program) : program = *) 
