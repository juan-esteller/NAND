{ 
  open Parser ;;

  let symTable =
    Hashtbl.create 30

  let _ = List.iter  (fun (kwd, tok) -> Hashtbl.add symTable kwd tok)
                       ([ (",", COMMA); ]) 
}


let bin_str = ['0' - '1']+   

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* skip whitespace *)
  | "," { COMMA } 
  | ['0' - '1']+ as str { STRING(str)  }  
  | eof { EOF }
