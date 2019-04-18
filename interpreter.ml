exception TypeError of string

let rec string_of_list l =
match l with
| [] -> ""
| h::t -> h ^ ", " ^ (string_of_list t)

let rec string_of_expr = function
  | Expression.Fact (e1, e2)  -> e1  ^ "[" ^ (string_of_list e2) ^ "]"

(* Interpreter *)
let rec eval e env : unit =
	match e with
		| Expression.Fact (name, arglist) -> 
			let env'  = (Env.addBinding name (Expression.Fact (name, arglist)) env) in ()

let rec query e env : string =
	match e with
		| Expression.Fact (name ,arglist) ->
			let fact = (Env.apply name env) in
				match fact with
				| Expression.Fact (name, origlist) ->
					if arglist=origlist then "true" else "false"
				| _ -> "no such fact"
