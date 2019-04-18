(* Expression - begin *)
type expr =
  | Sid        of string
  | Bid		of string
  | IntConst  of int
  | Fact of string * string list;;
(* Expression - end *)

