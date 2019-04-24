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

let rec isPresent x env =
  match env with
    EmptyEnv -> false
  | NonEmptyEnv((key, value), env') ->
    if x = key then true
    else (isPresent x env')

let resolveVariableIfPossible env x =
    match x with
| Constant(_) -> x
| Variable(var) ->
    if isPresent var env then apply var env
    else x

(* Environment - end *)

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




let database = [
    Rule(PredicateName("fact1"), [Constant(Atom("a"))], []);
    Rule(PredicateName("fact1"), [Constant(Atom("b"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("a")); Constant(Atom("b"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("b")); Constant(Atom("c"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("d")); Constant(Atom("e"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("f")); Constant(Atom("g"))], []);
    Rule(PredicateName("fact3"), [Variable("A"); Variable("B")], [Query(PredicateName("fact2"), [Variable("A"); Variable("C")]); Query(PredicateName("fact2"), [Variable("C"); Variable("B")])])
    ];;

let query1 = Query(PredicateName("fact1"), [Constant(Atom("a"))]);;
let query2 = Query(PredicateName("fact3"), [Variable("a"); Variable("b")]);;

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
      Constant(_), Constant(_) -> (queryArg::unifiedArgList, env)
    | Constant(_), Variable(varName) ->(
            let env = addBinding varName queryArg env in
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


let rec constructQueryFromEnv query env =
    match query with
        Query(qName , argList) -> Query(qName, List.map (resolveVariableIfPossible env) argList)

let bindQueryResultToEnv query queryResponseList env =
    let rec _bindQueryResultToEnv argList queryResponseList env =
        match argList, queryResponseList with
            [], _ -> env
        |   Constant(_)::t1, h::t2 -> _bindQueryResultToEnv t1 t2 env
        |   Variable(varName)::t1, value::t2 -> let env = addBinding varName value env in _bindQueryResultToEnv t1 t2 env
    in match query with
    Query(_, argList) -> _bindQueryResultToEnv argList queryResponseList env
        ;;

let rec resolveQuery query database =
    let rec resolveRule ruleQueryList env unifiedArgList =
        match ruleQueryList with
            [] -> (true, env)
        |   query::t ->
            let bindedQuery = (constructQueryFromEnv query env) in
            let (status, queryResponseList) = _resolveQuery bindedQuery database in
                if status = false then (status, env)
                else
                    let env = bindQueryResultToEnv bindedQuery queryResponseList env in
                        resolveRule t env unifiedArgList
        and
     _resolveQuery _query _database =
        match _database with
            [] -> (false, [])
            | rule::t ->
                match rule, _query with
                Rule(predicateName, predicateArgList, ruleQueryList), Query(queryName, queryArgList) ->(
                    if predicateMatch _query rule then
                        let (unifiedArgList, env) = unify _query rule
                            in let (status, env) = resolveRule ruleQueryList env unifiedArgList in
                                if status then
                                    (status, List.map (resolveVariableIfPossible env) unifiedArgList)
                                else _resolveQuery _query t
                    else
                        _resolveQuery _query t
                        )
    in _resolveQuery query database;;


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
