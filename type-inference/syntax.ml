(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | LogAnd | LogOr

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | ErrorExp of string
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp
  | InfixExp of binOp

type program = 
    Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp

type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

let rec pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar (var) -> print_string (Printf.sprintf "'%c" (char_of_int ((int_of_char 'a') + var)))
  | TyFun (ty1, ty2) -> 
      (match ty1 with
        TyFun (_, _) -> print_string "("; pp_ty ty1; print_string ") -> "; pp_ty ty2;
      | _ -> pp_ty ty1; print_string " -> "; pp_ty ty2;)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
      counter := v + 1; v
  in body

let rec freevar_ty ty = 
  (match ty with
      TyVar (tyvar) -> MySet.singleton tyvar
    | TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)
    | _ -> MySet.empty)
