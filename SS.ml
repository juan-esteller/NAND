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
  | _ -> false

(* maps a macro that maps a command to a sequence of commands to entire program *)
let mapToProg (macro: command -> program) (p: program): program =
  List.concat (List.map macro p)



(* END OF HELPER FUNCTIONS *)

module type SS_module = sig
  val addSS : program -> program 
end 

(* START OF MACROS, in order of increasing complexity *)
module SSFromBackEnd (Lang: PL_back_end) : SS_module = 
struct 


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

let rec expandExp (e: exp) : program * exp =
  if isValue e then
    ([], e)
  else
    match e with
    | Binop(b, e1, e2) ->
        let v = freshVar () in
          let (p1, e1'), (p2, e2') = expandExp e1, expandExp e2 in
            (p1 @ (p2 @ [Asg([v], [Binop(b, e1', e2')])]), Var(v))
     | FxnApp(id, args) ->
         let argList = List.map expandExp args in
             let processArg ((p, e): program * exp) ((accProg, accArg): program * (exp list)) : program * (exp list)  =
               (p @ accProg, e :: accArg)
             in let p, newArgs = List.fold_right processArg argList ([], [])  in
           let v = freshVar () in
             (p @ [Asg([v], [FxnApp(id, newArgs)])], Var(v))
     | _ -> ([], e)

exception Impossible
(* enables direct assignment (i.e. a := b)  *)
let enableAsgCom (c: command) : program =
  match c with
  | Asg([u], [e]) ->
     if isValue e && not Lang.supportsAsg then
      let newVar, eStr =  freshVar (), strOfExp e in
        let newVarStr = strOfId newVar in
         (genLine newVar eStr eStr) @ (genLine u newVarStr newVarStr)
     else (match e with
           | Binop(b, e1, e2) ->
              let (p1, e1'), (p2, e2') = expandExp e1, expandExp e2 in
                p1 @ (p2 @ [Asg([u], [Binop(b, e1', e2')])])
           | FxnApp(_id, _args) -> [c]
           | _ -> [c])  
  | _ -> [c]

let enableAsgProg (p: program) : program =
  mapToProg enableAsgCom p

(* module for handling stores of functions *)
module FuncMap = Map.Make(String)

(* function store is a hashtable from function IDs to functions *)
type funcStore =  func FuncMap.t

let substProg (outList: (varID * varID) list)  (argList: (varID * varID) list) (p: program) : program =
  let substId (subsList: (varID * varID) list) (id: varID) : varID =
    try
     List.assoc id subsList
    with Not_found ->
     let body, ind = id in
       if body = "i" then 
          id 
       else 
         ("up"^body, ind)
  in let substExp (e: exp) : exp =
    match e with
    | Var(id) -> 
       let body, _ind = id in 
         if body = "i" && Lang.supportsI then 
           Var(id) 
         else 
           Var(substId (outList @ argList) id)
    | _ -> e
  in let substCom (c: command) : command =
    match c with
    | Asg([h], [Binop(b, e1, e2)]) -> Asg([substId outList h], [Binop(b, substExp e1, substExp e2)])
    | Asg([h], [Var(id)]) -> Asg([substId outList h], [substExp (Var(id))])
    | _ -> raise Invalid_command
  in List.map substCom p

exception Invalid_input of exp

let strip (e: exp) : varID =
  match e with
  | Var(id) -> id
  | _ -> raise (Invalid_input(e))

let expandFunc (ids: varID list) (args: exp list) (f: func): program =
  let zip = List.map2 (fun x y -> (x, y)) in
    let outList, argsList = (zip f.outputs ids), (zip f.inputs (List.map strip args)) in
      substProg outList argsList f.body

exception Unbound_function of funcID

(* processes a line in a program with function definitions *)
let rec enableFuncCom  (st: funcStore ref) (c: command) : program =
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
  | If(b, body) -> 
      let curStore = !st in 
        let newBody = mapToProg (enableFuncCom st) body in 
          (st := curStore); [If(b, newBody)] 
  | _ -> [c]


let enableFuncProg (p: program) : program =
  let st = ref FuncMap.empty in
    mapToProg (enableFuncCom st) p

let enableNestedFuncCom (c: command) : program =
  match c with
  | Asg([id], [FxnApp(fId, args)]) ->
  let argList = List.map expandExp args in
      let processArg ((p, e): program * exp) ((accProg, accArg): program * (exp list)) : program * (exp list)  =
        (p @ accProg, e :: accArg)
      in let p, newArgs = List.fold_right processArg argList ([], [])  in
        p @ [Asg([id], [FxnApp(fId, newArgs)])]
  | _ -> [c]

let enableNestedFuncProg (p: program) : program =
  mapToProg enableNestedFuncCom p

let enableNestedIfCom (c: command) : program =
  match c with
  | If(exp, prog) ->
      let p, id = expandExp exp in
        p @ [If(id, prog)]
  | _ -> [c]

let enableNestedIfProg (p: program) : program =
  mapToProg enableNestedIfCom p

let enableMUX (b: varID) ((l, r):  (varID * varID)) : program =
  let bStr, lStr, rStr = strOfId b, strOfId l, strOfId r in
   let comStr = lStr^" := MUX("^bStr^", "^lStr^", "^rStr^")" in
     parseStr comStr

let expandMUX: program ->  program =
  let muxDefProg = parseStr Lang.muxDefStr in
    fun (p: program) -> enableFuncProg (muxDefProg @ p)

exception Internal_error

let restoreIProg (v: varID) (b: varID) : program = 
    let varStr, bStr = strOfId v, strOfId b in 
      let progStr = 
       "tmp_0 := "^varStr^
       "  tmp_1 := i"^ 
       " i := "^bStr^
       "  i := tmp_i" 
      in parseStr progStr    

let expandIf (e: exp) (p: program) : program =
  let handleCom (b: varID) (c: command) : program =
    match c with
    | Asg([h], [e])->
        if h = ("i", Int(0)) then 
            [c]   
        else let newH = freshVar () in 
            [Asg([newH], [e])] @ (enableMUX b (h, newH))  
    | _ -> raise (Invalid_command)
  in let b = strip e in
      let newProg = List.concat (List.map (handleCom b) p)  in
        let origLine, endLine= 
       if not Lang.supportsAsg then 
          [], []
       else 
          let temp = freshVar () in
             let tempStr, bStr = strOfId temp, strOfId b in 
               parseStr (tempStr^":= i"), restoreIProg temp b  
      in origLine @ (expandMUX newProg) @ endLine

let rec enableIfProg (p: program) : program =
  mapToProg enableIfCom p
and enableIfCom (c: command) : program =
  match c with
  | If(b, body) ->
      expandIf b (enableIfProg body)
  | FxnDef(fId, f) ->
      [FxnDef(fId, {f with body = enableIfProg f.body})]
  | _ -> [c]

(* other macros to be applied to program, in order of their application *)
let macroList = [unzipProg; enableAsgProg; enableNestedFuncProg; enableNestedIfProg]

let rec enableNestedApp (m: program -> program) (p: program) =
  let p' = m p in
    let enterNestedProgs (c: command) : command =
      match c with
      | If(b, prog) -> If(b, enableNestedApp m prog)
      | FxnDef(id, f) -> FxnDef(id, {f with body = (enableNestedApp m f.body)})
      | _ -> c
    in List.map enterNestedProgs p'

let otherMacros =
  List.fold_left (fun acc f -> (fun p -> f (acc p))) (fun x -> x)
                 (List.map enableNestedApp macroList)

let addSS (p: program) : program =
  enableFuncProg (enableIfProg (otherMacros p))  

end 
(*
let constProg =
  "notx_0 := x_0 NAND x_0
   one := notx_0 NAND x_0
   zero := one NAND one"
(* assumes that program has already expanded all function applications *)
let enableConst (p: program) : program = *)
