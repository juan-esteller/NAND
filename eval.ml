let () =
  let _ =
    if Array.length Sys.argv <> 3 then
     ((Format.printf "Usage: nand <file> <flags> <binary input>\n"); exit 0) in
  let fileName = Sys.argv.(1) in
  let startExt = (String.rindex fileName '.') + 1 in
  let ext = String.sub fileName startExt ((String.length fileName) - startExt) in
    let _ =
      if ext <> "nand" then
       ((Format.printf "Must provide a .nand file as input\n"); exit 0) in
  let file = open_in fileName in
  let lexbuf = Lexing.from_channel file in
  let program = NANDparser.input NANDlexer.token lexbuf in
  let output = Nand.evaluate program Sys.argv.(2) true in
      Printf.printf "Output is %s\n" output
