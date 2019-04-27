let evaluate () =
  try
    let cin =
      if Array.length Sys.argv > 1 then
        open_in Sys.argv.(1)
      else
        stdin
     and  database = [] in
    let e1 =
      let lexbuf = Lexing.from_channel cin in
        Parser.predicate Lexer.scan lexbuf
    in
        database = e1::database;
    let database = database in
	() (*print_string result*)
  with End_of_file -> exit 0

let _ = evaluate ()
