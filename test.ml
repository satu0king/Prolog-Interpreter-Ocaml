type value =
    | Integer of int
    | Atom of string;;

type argument =
    | Constant of value
    | Variable of string;;

type predicateName = PredicateName of string;;

type predicate =
    | Fact of predicateName * argument list
    | Rule of predicateName * argument list * predicate list
    | LessThan of argument * argument
    | GreaterThan of argument * argument
    | LessThanOrEqualTo of argument * argument
    | GreaterThanOrEqualTo of argument * argument
    | Is of argument * argument
    | Equal of argument * argument
    | IsNot of argument * argument;;

type query = {
	name : predicateName;
	arguments :  argument list;
}

let resolvePredicate predicate =
    match predicate with
    | LessThan(Constant(v1), Constant(v2)) ->
        v1 < v2
    | GreaterThan(Constant(v1), Constant(v2)) ->
        v1 > v2
    | LessThanOrEqualTo(Constant(v1), Constant(v2)) ->
        v1 <= v2
    | GreaterThanOrEqualTo(Constant(v1), Constant(v2)) ->
        v1 >= v2
    | Equal(Constant(v1), Constant(v2)) ->
        v1 == v2;;

let resolveQuery query =
    match query with

let p1 = LessThan(Constant(Integer(3)), Constant(Integer(2)));;
let p2 = LessThan(Constant(Integer(1)), Constant(Integer(2)));;

resolvePredicate p1;;
resolvePredicate p2;;
