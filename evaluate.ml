
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
    let solnGenerator = Interpreter.resolveQuery query database in
    let matchFound = ref false in
    try while true do
        let (status, arguments) = solnGenerator() in
            Printf.printf "\t = %s\n"  (Interpreter.printResult query arguments);
            matchFound:= true;
            flush stdout;
        done
    with Failure _ -> (
        if not !matchFound then (
            Printf.printf (if Interpreter.containsVarible query then
                 "\t = No solution \n"
            else  "\t = False \n");
            flush stdout
        )
        )
    done

let _ = evaluate ()
