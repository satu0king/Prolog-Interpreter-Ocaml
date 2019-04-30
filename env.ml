
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
| Expression.Constant(_) -> x
| Expression.Variable(var) ->
    if isPresent var env then apply var env
    else x

(* Environment - end *)
