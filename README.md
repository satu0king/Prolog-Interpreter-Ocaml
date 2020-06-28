# Prolog-Interpreter-Ocaml

## Instructions to compile and run the code
```
make
./evaluate
```
The first line of the input when the program is run MUST be the database file. For example,
```
| ?- ['database.pl'].
```
## Brief description of the files
1. env.ml contains the functions related to the variant type env.
2. evaluate.ml contains the "main" function which interacts with the user.
3. expression.ml contains all the expression variant types.
4. interpreter.ml contains the prolog execution engine.
5. lexer.mll contains the specification of the tokens needed by ocamllex.
6. parser.mly contains the grammar needed by ocamlyacc.
