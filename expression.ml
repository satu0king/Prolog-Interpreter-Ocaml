

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



let database = [
    Rule(PredicateName("fact1"), [Constant(Atom("a"))], []);
    Rule(PredicateName("fact1"), [Constant(Atom("b"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("a")); Constant(Atom("b"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("b")); Constant(Atom("c"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("c")); Constant(Atom("d"))], []);
    Rule(PredicateName("fact2"), [Constant(Atom("d")); Constant(Atom("e"))], []);
    Rule(PredicateName("fact3"), [Variable("A"); Variable("B")], [Query(PredicateName("fact2"), [Variable("A"); Variable("C")]); Query(PredicateName("fact2"), [Variable("C"); Variable("B")])]);
    Rule(PredicateName("fact4"), [Variable("A"); Variable("B")], [Query(PredicateName("fact2"), [Variable("A"); Variable("B")])]);
    Rule(PredicateName("fact4"), [Variable("A"); Variable("B")], [Query(PredicateName("fact2"), [Variable("A"); Variable("C")]); Query(PredicateName("fact4"), [Variable("C"); Variable("B")])])
    ];;

let query1 = Query(PredicateName("fact1"), [Variable("X")]);;
let query2 = Query(PredicateName("fact3"), [Constant(Atom("b")); Constant(Atom("d"))]);;
let query3 = Query(PredicateName("fact4"), [Constant(Atom("a")); Constant(Atom("e"))]);;

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
                                if status = true then
                                    (status, List.map (resolveVariableIfPossible env) unifiedArgList)
                                else _resolveQuery _query t
                    else
                        _resolveQuery _query t
                        )
    in _resolveQuery query database;;

let dummyQuerySolnGenerator () =
        let fn() = (
            failwith "No Match";
            (true, []);
            )
    in fn

let rec resolveQuery query database =
    let rec resolveRule ruleQueryList env unifiedArgList = (
        let rule_solution_generators = Array.make (List.length ruleQueryList)
            (_resolveQuery
                (constructQueryFromEnv (List.hd ruleQueryList) env)
                 database) in
        let rule_env = Array.make (List.length ruleQueryList ) env in
        let rule_index = ref 0 in
        let answerGenerator() = (
            while !rule_index != (List.length ruleQueryList) do
                try
                    let (status, queryResponseList) = rule_solution_generators.(!rule_index)() in (
                        rule_env.(!rule_index) <- bindQueryResultToEnv (List.nth ruleQueryList !rule_index ) queryResponseList rule_env.(!rule_index);
                        if !rule_index < (List.length ruleQueryList) then (
                            rule_index:= !rule_index + 1;
                            rule_solution_generators.(!rule_index) <-
                                _resolveQuery (constructQueryFromEnv (List.nth ruleQueryList !rule_index) rule_env.(!rule_index-1)) database);

                    )
                with
                    Failure(_) -> (!rule_index = !rule_index -1);

                if !rule_index = -1 then
                    failwith "No Match"

                (* let (status, queryResponseList) = (_resolveQuery bindedQuery database)() in
                    if status = false then (status, env)
                    else
                        let env = bindQueryResultToEnv bindedQuery queryResponseList env in
                            resolveRule t env unifiedArgList *)
            done;
            (true, rule_solution_generators.(!rule_index - 1))
        )
        in
        answerGenerator()
    )
    and _resolveQuery query database = (
        let predicate_matches = Array.of_list( List.filter (predicateMatch query) database) and
        predicate_index = ref (-1) in
        let ruleSolnGenerator = ref (dummyQuerySolnGenerator()) in
        let answerGenerator() = (
        if !predicate_index = Array.length predicate_matches then failwith "No Match"
        else
        match predicate_matches.(!predicate_index), query with

            
            Rule(predicateName, predicateArgList, ruleQueryList), Query(queryName, queryArgList) ->(
                    let (unifiedArgList, env) = unify query predicate_matches.(!predicate_index) in
                        predicate_index:= !predicate_index+1;
                        (true, unifiedArgList);
                    )
        )

    in answerGenerator)
    in
    _resolveQuery query database;;

let string_of_constant c =
    match c with
    Integer(i) -> string_of_int i
    | Atom(a) -> a

let printResult query result =
    let rec _printResult queryList resultList =
        match queryList, resultList with
            | [], [] -> ""
            | Variable(v)::t1, Constant(c)::t2 ->" | " ^ v ^ " = " ^ (string_of_constant c) ^ (_printResult t1 t2)
            | _::t1, _::t2 -> _printResult t1 t2
    in
        match query with
        Query(_, queryList) -> _printResult queryList result
