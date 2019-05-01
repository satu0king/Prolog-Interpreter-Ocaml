
open Expression;;

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
let query2 = Query(PredicateName("fact3"), [Variable("X"); Variable("Y")]);;
let query3 = Query(PredicateName("fact4"), [Variable("X"); Variable("Y")]);;

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
            let env = Env.addBinding varName queryArg env in
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
                let (unifiedArgList, env) = _unify queryArgumentList predicateArgList [] (Env.emptyEnv())
                    in (List.rev unifiedArgList, env);;

let rec constructQueryFromEnv query env =
    match query with
        Query(qName , argList) -> Query(qName, List.map (Env.resolveVariableIfPossible env) argList)

let bindQueryResultToEnv query queryResponseList env =
    let rec _bindQueryResultToEnv argList queryResponseList env =
        match argList, queryResponseList with
            [], _ -> env
        |   Constant(_)::t1, h::t2 -> _bindQueryResultToEnv t1 t2 env
        |   Variable(varName)::t1, value::t2 -> let env = Env.addBinding varName value env in _bindQueryResultToEnv t1 t2 env
    in match query with
    Query(_, argList) -> _bindQueryResultToEnv argList queryResponseList env
        ;;

let dummyQuerySolnGenerator () =
        let fn() = (
            failwith "No Match";
            (true, (Env.emptyEnv()));
            )
    in fn

let rec resolveQuery query database =
    let rec resolveRule ruleQueryList env unifiedArgList = (
        let rule_solution_generators = Array.make (List.length ruleQueryList)
            (_resolveQuery
                (constructQueryFromEnv (List.hd ruleQueryList) env)
                 database) in
        let rule_env = Array.make ((List.length ruleQueryList ) +1) env in
        let rule_index = ref 0 in
        let answerGenerator() = (

            if !rule_index = (List.length ruleQueryList) then
            (
                try
                    let (status, queryResponseList) = rule_solution_generators.(!rule_index-1)() in
                        rule_env.(!rule_index) <- bindQueryResultToEnv (List.nth ruleQueryList (!rule_index-1) ) queryResponseList rule_env.(!rule_index);

                with
                    Failure _ -> (rule_index := !rule_index -1);
            );



            while !rule_index != (List.length ruleQueryList) do

                try
                    let (status, queryResponseList) = rule_solution_generators.(!rule_index)() in (
                        rule_env.(!rule_index + 1) <- bindQueryResultToEnv (List.nth ruleQueryList !rule_index ) queryResponseList rule_env.(!rule_index);
                        rule_index:= !rule_index + 1;
                        if !rule_index < ((List.length ruleQueryList)) then (
                            rule_solution_generators.(!rule_index) <-
                                _resolveQuery (constructQueryFromEnv (List.nth ruleQueryList !rule_index) rule_env.(!rule_index)) database
                            );


                    )
                with
                    Failure _ -> (rule_index := !rule_index -1);

                if !rule_index = -1 then
                    failwith "No Match"

            done;
            (true, rule_env.(!rule_index))
        )
        in
        answerGenerator
    )
    and _resolveQuery query database = (
        let predicate_matches = Array.of_list( List.filter (predicateMatch query) database) and
        predicate_index = ref (-1) in
        let ruleSolnGenerator = ref (dummyQuerySolnGenerator()) in
        let unifiedList = ref ([]) in
        let answerGenerator() = (
        if !predicate_index = Array.length predicate_matches then failwith "No Match"
        else

        let matchFound = ref false in
        let soln = ref (false, Env.emptyEnv()) in
        (
        while not !matchFound do
                try
                    soln := !ruleSolnGenerator();
                    matchFound := true;
                with Failure _ ->(
                    predicate_index:= !predicate_index+1;
                    if !predicate_index = Array.length predicate_matches then failwith "No Match";
                    match predicate_matches.(!predicate_index), query with
                        Rule(predicateName, predicateArgList, ruleQueryList), Query(queryName, queryArgList) ->(
                                let (unifiedArgList, env) = unify query predicate_matches.(!predicate_index) in
                                    ruleSolnGenerator:= (
                                    if List.length ruleQueryList = 0 then
                                        (dummyQuerySolnGenerator())
                                    else
                                      resolveRule ruleQueryList env unifiedArgList);
                                    if List.length ruleQueryList = 0 then
                                        matchFound := true;
                                    unifiedList:=  unifiedArgList;
                                )
                )
            done;

            if !predicate_index = Array.length predicate_matches then failwith "No Match";

            (true, List.map (Env.resolveVariableIfPossible (snd !soln)) !unifiedList)
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
            | [], [] -> []
            | Variable(v)::t1, Constant(c)::t2 -> (v ^ " = " ^ (string_of_constant c) ):: (_printResult t1 t2)
            | _::t1, _::t2 -> _printResult t1 t2
    in
        match query with
         Query(_, queryList) -> let result = _printResult queryList result in
            if List.length result = 0 then  "True" else String.concat ", " result;;

let containsVarible query =
    match query with
        Query(_, argList) ->
            let rec _containsVarible argList =
                match argList with
                    [] -> false
                    | Variable(_)::t -> true
                    | Constant(_)::t->_containsVarible t
            in
                _containsVarible argList;;
