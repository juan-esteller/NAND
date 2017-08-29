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

(* enables nested application of a macro by applying it to all 
   sub-programs contained in function defs and if-statements *)
let rec enableNestedApp (m: program -> program) (p: program) =
  let p' = m p in
    let enterNestedProgs (c: command) : command =
      match c with
      | If(b, prog) -> If(b, enableNestedApp m prog)
      | FxnDef(id, f) -> FxnDef(id, {f with body = (enableNestedApp m f.body)})
      | _ -> c
    in List.map enterNestedProgs p'


(* END OF HELPER FUNCTIONS *)

(* module signature for syntactic sugar module *) 
module type SS_module = sig
  val addSS : program -> program 
end 

(* functor that takes in a PL_back_end and generates its SS module *) 
module SSFromBackEnd (Lang: PL_back_end) : SS_module = 
struct 

(* macro to add standard library to a program *)  
let addStdLib (p: program) : program = 
  let stdLib = 
    "one := zero NAND zero 
     def a := NOT(b) { 
       a := b NAND b 
     }
     def a := AND(b, c) { 
       a := NOT(b NAND c) 
     }" 
  in (parseStr stdLib) @ p 

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

(* ditto, but for entire program *) 
let unzipProg (p: program) : program =
  mapToProg unzipCom p

(* recursively expands an expression so that every assignment
   is of form id_1 := id_2 BINOP id_3, and returns 
   id that eventually receives value of expression *) 
let rec expandExp (e: exp) : program * exp =
  (* if expression is already a value, returns it *) 
  if isValue e then
    ([], e)
  else
    match e with 
    (* in case a binop, expands left and right arguments, returns *) 
    | Binop(b, e1, e2) ->
        let v = freshVar () in
          let (p1, e1'), (p2, e2') = expandExp e1, expandExp e2 in
            (p1 @ (p2 @ [Asg([v], [Binop(b, e1', e2')])]), Var(v))
    (* expands every argument, then puts in their resultant IDs *)  
    | FxnApp(id, args) ->
         let argList = List.map expandExp args in
             let processArg ((p, e): program * exp) 
                   ((accProg, accArg): program * (exp list)) : program * (exp list)  =
               (p @ accProg, e :: accArg)
             in let p, newArgs = List.fold_right processArg argList ([], [])  in
           let v = freshVar () in
             (p @ [Asg([v], [FxnApp(id, newArgs)])], Var(v))
     | _ -> ([], e)

exception Impossible

(* enables assignment of any form for commands *)  
let enableAsgCom (c: command) : program =
  (* in the case it finds direct assignment, expands it to two lines if needed *) 
  match c with
  | Asg([u], [e]) ->
     if isValue e && not Lang.supportsAsg then
      let newVar, eStr =  freshVar (), strOfExp e in
        let newVarStr = strOfId newVar in
         (genLine newVar eStr eStr) @ (genLine u newVarStr newVarStr)
     (* for binary operations, expands both arguments *) 
     else (match e with
           | Binop(b, e1, e2) ->
              let (p1, e1'), (p2, e2') = expandExp e1, expandExp e2 in
                p1 @ (p2 @ [Asg([u], [Binop(b, e1', e2')])])
           | FxnApp(_id, _args) -> [c]
           | _ -> [c])  
  | _ -> [c]

(* enables assignment of any form for entire program *) 
let enableAsgProg (p: program) : program =
  mapToProg enableAsgCom p

(* module for handling stores of functions *)
module FuncMap = Map.Make(String)

(* function store is a hashtable from function IDs to functions *)
type funcStore =  func FuncMap.t

(* takes in function body, argument list, and output list, and performs appropriate substitutions *) 
let substProg (outList: (varID * varID) list) 
              (argList: (varID * varID) list) (p: program) : program =
  (* helper function that substitutes for an ID in the body given a list of substitutions  *) 
  let substId (subsList: (varID * varID) list) (id: varID) : varID =
    (* looks for in id list of substitution *) 
    try
      List.assoc id subsList
    with 
    | Not_found ->
        let body, ind = id in
          (* "i" is a global variable, so it's never changed *) 
          if body = "i" then
            id
          else  
          (* prepends "up" to prevent shadowing *) 
            ("up"^body, ind)
   (* helper function that substitutes in expressions *) 
   in let substExp (e: exp) : exp =
        match e with 
        | Var(id) ->
            let body, _ind = id in
              if body = "i" && Lang.supportsI then
                Var(id)
              else
                Var(substId (outList @ argList) id) 
        | _ -> e
   (* helper function that substitutes for commands *) 
   in let substCom (c: command) : command =
      (* NOTE: when we're substituting for variables that we're assigning to, 
         we only use output list, so that function argumetns remain immutable *) 
      match c with
      | Asg([h], [Binop(b, e1, e2)]) -> Asg([substId outList h], [Binop(b, substExp e1, substExp e2)])
      | Asg([h], [Var(id)]) -> Asg([substId outList h], [substExp (Var(id))])
      | _ -> raise Invalid_command
   in List.map substCom p

exception Invalid_input of exp

(* strips expression that should be variable of wrapper *) 
let strip (e: exp) : varID =
  match e with
  | Var(id) -> id
  | _ -> raise (Invalid_input(e))

(* takes in output list of function, arguments, and function record, 
   and returns corresponding program *)  
let expandFunc (ids: varID list) (args: exp list) (f: func): program =
 (* takes two lists and creates list of tuples *)  
 let zip = List.map2 (fun x y -> (x, y)) in
    (* associates inputs with input IDs, ditto for arguments *) 
    let outList, argsList = (zip f.outputs ids), (zip f.inputs (List.map strip args)) in
      substProg outList argsList f.body

exception Unbound_function of funcID

(* processes a line in a program with function definitions; 
   takes in as argument a command, and reference to store 
   of defined functions  *)
let rec enableFuncCom  (st: funcStore ref) (c: command) : program =
  match c with
  (* in case it finds a function ID, expands all function in body
     (passing in new reference for scoping), and then adds function 
     to top-level store *) 
  | FxnDef(fId, f) ->
     let curStore = !st in
       let newBody = mapToProg (enableFuncCom st) f.body in
         let newFun = {f with body = newBody} in
           (st := FuncMap.add fId newFun  curStore); []
  (* if it finds a function application, tries to expand it *) 
  | Asg(ids, [FxnApp(fId, args)]) ->
     (try
       expandFunc ids args (FuncMap.find fId !st)
      with 
      | Not_found -> [c]) 
  (* in case it finds if-statement, enters its body *) 
  | If(b, body) -> 
      let curStore = !st in 
        let newBody = mapToProg (enableFuncCom st) body in 
          (st := curStore); [If(b, newBody)] 
  | _ -> [c]

(* maps enableFuncCom to entire program to expand functions *) 
let enableFuncProg (p: program) : program =
  let st = ref FuncMap.empty in
    mapToProg (enableFuncCom st) p

(* enables arbitrary expressions as function arguments for individual commands *) 
let enableNestedFuncCom (c: command) : program =
  match c with
  | Asg([id], [FxnApp(fId, args)]) ->
  let argList = List.map expandExp args in
      let processArg ((p, e): program * exp) ((accProg, accArg): program * (exp list)) : program * (exp list)  =
        (p @ accProg, e :: accArg)
      in let p, newArgs = List.fold_right processArg argList ([], [])  in
        p @ [Asg([id], [FxnApp(fId, newArgs)])]
  | _ -> [c]

(* ditto, for programs *) 
let enableNestedFuncProg (p: program) : program =
  mapToProg enableNestedFuncCom p

(* function to append "up" to all function workspace variables
   that already have prefix "up"  to prevent shadowing for 
   if-statements (necessary for transformation for nested if-statements in NAND<< *) 
let preventShadowing (p: program) : program = 
  let handleId (id: varID) : varID = 
    let body, ind = id in 
      if (not (String.length body < 2)) && String.sub body 0 2 = "up" then 
        "up"^body, ind 
      else 
         id
  in let handleVar (e: exp) : exp = 
     match e with 
     | Var(id) -> Var(handleId id) 
     | _ -> raise Invalid_expression   
  in let handleCom (c: command) : command = 
     match c with 
     | Asg([h], [Binop(b, Var(id1), Var(id2))]) ->
         Asg([handleId h], [Binop(b,Var(handleId id1), Var(handleId id2))])
     | Asg([h], [Var(id)]) -> 
         Asg([handleId h], [Var(handleId id)]) 
     | Asg(outList, [FxnApp(name, argList)]) -> 
        let handleList = List.map handleId in 
          Asg(handleList outList, [FxnApp(name, List.map handleVar argList)])
     | _ -> raise Invalid_command 
  in List.map handleCom p

(* enables arbitrary expressions as predicate for if-statements *) 
let enableNestedIfCom (c: command) : program =
  match c with
  | If(exp, prog) ->
      let p, id = expandExp exp in
        p @ [If(id, prog)]
  | _ -> [c]

(* maps to entire program *) 
let enableNestedIfProg (p: program) : program =
  mapToProg enableNestedIfCom p

(* creates MUX application, where b is predicate and l 
   may be updated to have the value of r *) 
let enableMUX (b: varID) ((l, r):  (varID * varID)) : program =
  let bStr, lStr, rStr = strOfId b, strOfId l, strOfId r in
   let comStr = lStr^" := MUX("^bStr^", "^lStr^", "^rStr^")" in
     parseStr comStr

(* expands MUX applications left lying around in program; 
   written in this manner to prevent re-parsing *) 
let expandMUX: program ->  program =
  let muxDefProg = parseStr Lang.muxDefStr in
    fun (p: program) -> enableFuncProg (muxDefProg @ p)

exception Internal_error
(* function to restore value of i after potential usage by 
   MUX function *) 
let restoreIProg (v: varID) (b: varID) : program = 
    let varStr, bStr = strOfId v, strOfId b in 
      let progStr = 
       "tmp_0 := "^varStr^
       "  tmp_1 := i"^ 
       " i := "^bStr^
       "  i := tmp_i" 
      in parseStr progStr    

(* expands if-statement with body p, predicate e *) 
let expandIf (e: exp) (p: program) : program =
  (* interleaves MUX into lines of program to update variables properly *) 
  let handleCom (b: varID) (c: command) : program =
    match c with
    | Asg([h], [e])->
        if h = ("i", Int(0)) then 
            [c]   
        else let newH = freshVar () in 
            [Asg([newH], [e])] @ (enableMUX b (h, newH))  
    | _ -> [c] 
  in let b = strip e in
      let newB = freshVar () in 
        let asg = enableAsgProg (parseStr ((strOfId newB)^" := "^(strOfId b))) in 
        let newProg = asg @ (List.concat (List.map (handleCom newB) p))  in
        let origLine, endLine= 
      (* if program uses i for MUX, adds appropriate bookkeeping to if-statements *)  
      if not Lang.supportsAsg then 
          [], []
       else  
          let temp = freshVar () in
             let tempStr, bStr = strOfId temp, strOfId b in 
               parseStr (tempStr^":= i"), restoreIProg temp newB  
      in origLine @ (expandMUX newProg) @ endLine

let rec enableIfProg (p: program) : program =
  mapToProg enableIfCom p
and enableIfCom (c: command) : program =
  match c with
  | If(b, body) ->
      expandIf b (preventShadowing (enableIfProg body))
  | FxnDef(fId, f) ->
      [FxnDef(fId, {f with body = enableIfProg f.body})]
  | _ -> [c]

(* unfortunately, these are best implemented as mutually recursive functions *) 
let rec genPreLoop (preloop : program) (finishedpreloop: varID) : program = 
  let b, asgn =  FxnApp("NOT", [Var(finishedpreloop)]), parseStr ((strOfId finishedpreloop)^" := one") in
    let body = asgn @ preloop in 
      [If(b, body)] 
and genLoop (e: exp) (finishedpreloop: varID) (finishedloop: varID) (itemp: varID)  (body: program)  = 
  let itempStr = strOfId itemp in 
  let recoveri = List.hd (parseStr ("i := "^itempStr)) in  
  let innerif1 = If(e, enableWhileProg (body @ (parseStr ("loop := one "^itempStr^" := i")))) in 
  let innerif2 =  
    let pred, asgn = FxnApp("NOT", [e]), parseStr ((strOfId finishedloop)^" := one loop := zero") in  
        If(pred, asgn)  
  in let pred = Var(finishedpreloop) in 
   [If(pred, [recoveri; innerif1; innerif2])] 
and genPostLoop (finishedloop: varID) (postloop : program) : program = 
    [If(Var(finishedloop), enableWhileProg postloop)] 
and expandWhile (e: exp) (preloop: program) (body: program) (postloop: program) : program = 
 let finishedpreloop, finishedloop, itemp  = freshVar (), freshVar (), freshVar () in
     let preloopcode, loopcode, postloopcode = 
       genPreLoop preloop finishedpreloop, 
       genLoop e finishedpreloop finishedloop itemp body,
       genPostLoop finishedloop postloop
     in  (preloopcode @ loopcode @ postloopcode) 
and enableWhileProg (p: program) : program =
  if Lang.supportsI then 
    p 
  else let left = ref [] in
  let fxndefs = ref [] in  
    let rec enableWhileProgHelp (p: program) : unit = 
      match p with 
      | [] -> () 
      | c::right -> 
        (match c with 
         | While(b, body) -> left := (expandWhile b !left (enableWhileProg body) right)  
         | If(b, body) -> 
             left := !left @ ([(If(b, enableWhileProg body))]); enableWhileProgHelp right
         | FxnDef(name, f) -> 
             let newDef = FxnDef(name, {f with body = (enableWhileProg f.body)}) in 
               begin
                (fxndefs := newDef :: !fxndefs); 
                (enableWhileProgHelp right);
               end 
         | _ ->
             left := (!left @ [c]);  (enableWhileProgHelp right))
  in (enableWhileProgHelp p); (List.rev !fxndefs) @ !left

(* other macros to be applied to program, in order of their application *)
let macroList = [unzipProg; enableAsgProg; enableNestedFuncProg; enableNestedIfProg]

let otherMacros =
  List.fold_left (fun acc f -> (fun p -> f (acc p))) (fun x -> x)
                 (List.map enableNestedApp macroList)

let addSS (p: program) : program =
  let p' =  (enableIfProg ((otherMacros (addStdLib p)))) in 
    otherMacros (enableFuncProg p')

end 
