type env =
    EmptyEnv
  | NonEmptyEnv of (string * Expression.expr) * env

val emptyEnv : unit -> env

val addBinding : string -> Expression.expr -> env -> env

val apply : string -> env -> Expression.expr
