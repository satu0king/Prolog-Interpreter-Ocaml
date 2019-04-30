

(* val string_of_expr : Expression.expr -> string
val eval : Expression.expr -> Env.env -> unit
val query : Expression.expr -> Env.env ->  string *)

(* val resolveQuery : Expression.query -> Expression.predicate list -> bool * Expression.argument list;; *)

val resolveQuery : Expression.query -> Expression.predicate list -> unit -> bool * Expression.argument list;;
val printResult : Expression.query -> Expression.argument list -> string;;
val dummyQuerySolnGenerator : unit -> unit -> bool * ('a, 'b) Env.env;;
