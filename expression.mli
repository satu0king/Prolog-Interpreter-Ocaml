

 type value =
     | Integer of int
     | Atom of string;;

 type argument =
     | Constant of value
     | Variable of string;;

 type predicateName = PredicateName of string;;

 type query = Query of predicateName * argument list;;

 type predicate =
     | Rule of predicateName * argument list * query list
     | LessThan of argument * argument
     | GreaterThan of argument * argument
     | LessThanOrEqualTo of argument * argument
     | GreaterThanOrEqualTo of argument * argument
     | Is of argument * argument
     | Equal of argument * argument
     | IsNot of argument * argument;;

type database = predicate list

type expr =
| Sid        of string
| Bid		of string
| IntConst  of int
| Fact of string * string list


val resolveQuery : query -> predicate list -> bool * argument list;;
