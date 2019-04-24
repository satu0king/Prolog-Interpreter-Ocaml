type value =
    | Integer of int
    | Atom of string;;

type argument =
    | Constant of value
    | Variable of string;;

type predicateName = PredicateName of string;;


(* Environment - begin *)
type ('a, 'b) env =
    EmptyEnv
  | NonEmptyEnv of ('a * 'b) * ('a, 'b) env

let emptyEnv () = EmptyEnv

let addBinding x v env =
  NonEmptyEnv((x, v), env)

let rec apply x env =
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((key, value), env') ->
    if x = key then value
    else (apply x env')


(* Environment - end *)

type predicate =
    | Rule of predicateName * argument list * predicate list
    | LessThan of argument * argument
    | GreaterThan of argument * argument
    | LessThanOrEqualTo of argument * argument
    | GreaterThanOrEqualTo of argument * argument
    | Is of argument * argument
    | Equal of argument * argument
    | IsNot of argument * argument;;


type query = Query of predicateName * argument list;;

let database = [
    Rule(PredicateName("fact1"), [Constant(Atom("a"))], []);
    Rule(PredicateName("fact1"), [Constant(Atom("b"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("b")); Constant(Atom("a"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("c")); Constant(Atom("d"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("a")); Constant(Atom("a"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("a")); Constant(Atom("c"))], [])
    ];;

let query1 = Query(PredicateName("fact1"), [Constant(Atom("a"))]);;
let query2 = Query(PredicateName("fact2"), [Variable("s"); Variable("b")]);;

let checkArgumentMatch argumentList argList =
    if List.length argumentList != List.length argList then false
    else
        let rec _checkArgumentMatch argumentList argList =
            match argumentList, argList with
                [], [] ->true
                | arg1::t1, arg2::t2 ->(
                    match arg1, arg2 with
                        Constant(c1), Constant(c2) -> if c1 = c2 then _checkArgumentMatch t1 t2 else false
                    | _, _ -> _checkArgumentMatch t1 t2 )
        in _checkArgumentMatch argumentList argList

    ;;


let predicateMatch query rule=
    match query, rule with
        Query(predicate, argumentList), Rule(predicateName, argList,_) ->
            if predicate = predicateName then checkArgumentMatch argumentList argList else false;;

let unifyArgument queryArg predicateArg unifiedArgList env =
    match queryArg, predicateArg with
      Constant(c), Constant(_) -> (queryArg::unifiedArgList, env)
    | Constant(c), Variable(varName) ->(
            let env = addBinding varName c env in
                (queryArg::unifiedArgList, env)
        )
    | Variable(varName), Constant(c) -> (predicateArg::unifiedArgList, env)
    | Variable(varName1), Variable(varName2) -> (predicateArg::unifiedArgList, env)
        ;;



let unify query predicate =
    let rec _unify  queryArgumentList predicateArgList unifiedArgList env =
        match queryArgumentList, predicateArgList with
            queryArg::t1, predicateArg::t2 ->
                    let (unifiedArgList, env) = unifyArgument queryArg predicateArg unifiedArgList env in
                        _unify t1 t2 unifiedArgList env
                    | _,_ -> (unifiedArgList, env)
    in
        match query, predicate with
            Query(predicate, queryArgumentList), Rule(predicateName, predicateArgList, _) ->
                let (unifiedArgList, env) = _unify queryArgumentList predicateArgList [] (emptyEnv())
                    in (List.rev unifiedArgList, env);;


let rec resolveQuery query database =
    let rec _resolveQuery _query _database =
        match _database with
            [] -> (false, [])
            | rule::t ->
                match rule, _query with
                Rule(predicateName, predicateArgList, _), Query(queryName, queryArgList) ->(
                    if predicateMatch _query rule then
                        let (unifiedArgList, env) = unify _query rule in (true, unifiedArgList)
                    else
                        _resolveQuery _query t)
    in _resolveQuery query database;;

    (* match query with
    | Query(predicate, argumentList) ->
            match database with
            [] -> False
        |   h::t ->
            match h with
            Rule(predicateName, argList, predList) ->
                if predicateName = predicate and (checkPredicateMatch argumentList argList) then
                    resolveQuery *)





(* let resolvePredicate predicate =
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
        v1 == v2;; *)

(* let resolveQuery query =
    match query with *)

(* let p1 = LessThan(Constant(Integer(3)), Constant(Integer(2)));;
let p2 = LessThan(Constant(Integer(1)), Constant(Integer(2)));; *)

(* resolvePredicate p1;;
resolvePredicate p2;; *)
