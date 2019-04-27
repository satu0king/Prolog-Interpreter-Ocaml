
let evaluate () =

    let cin =
      (if Array.length Sys.argv > 1 then
        open_in Sys.argv.(1)
      else
        stdin)
    in
    let _ = print_string "| ?- "
    in
    let _ = flush stdout
    in
    let filename =
      (let lexbuf = Lexing.from_channel cin in
        Parser.filename Lexer.scan lexbuf
    )
    in
    let file_handle = open_in filename
    in
    let database = (let lexbuf = Lexing.from_channel file_handle in
       Parser.database Lexer.scan lexbuf
    )
    in
    let lexbuf = Lexing.from_channel stdin in
    while true do
    print_string "| ?- ";
    flush stdout;
    let query = Parser.interpreter_query Lexer.scan lexbuf in
    print_newline();
    flush stdout;
    let (status, arguments) = Expression.resolveQuery query database in
        Printf.printf "\t = %s\n"  (string_of_bool status);
        if status then
            Printf.printf "\t = %s\n"  (Expression.printResult query arguments);
        flush stdout;
    done

let _ = evaluate ()
