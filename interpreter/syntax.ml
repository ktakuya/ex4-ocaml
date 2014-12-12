(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | LogAnd | LogOr

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | ErrorExp of string

type program = 
    Exp of exp
