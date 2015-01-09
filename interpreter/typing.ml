open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

let ty_prim op ty1 ty2 = match op with
    Plus -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
              | _ -> err ("Argument must be of integer: +"))
  | Mult -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
              | _ -> err ("Argument must be of integer: *"))
  | Lt -> (match ty1, ty2 with
                TyInt, TyInt -> TyBool
              | _ -> err ("Argument must be of interger: <"))
  | LogAnd -> (match ty1, ty2 with
                TyBool, TyBool -> TyBool
              | _ -> err ("Argument must be of boolean: &&"))
  | LogOr -> (match ty1, ty2 with
                TyBool, TyBool -> TyBool
              | _ -> err ("Argument must be of boolean: ||"))
  | _ -> err "Not Implemented!"

let rec ty_exp tyenv = function
    Var x ->
        (try Environment.lookup x tyenv with
            Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
        let tyarg1 = ty_exp tyenv exp1 in
        let tyarg2 = ty_exp tyenv exp2 in
            ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
        let tycond = ty_exp tyenv exp1 in
        let tyarg1 = ty_exp tyenv exp2 in
        let tyarg2 = ty_exp tyenv exp3 in
            (match tycond with
                TyBool -> (match tyarg1, tyarg2 with
                            TyInt, TyInt -> TyInt
                          | TyBool, TyBool -> TyBool
                          | TyInt, TyBool -> err ("This expression has type bool but an expression was expected of type int")
                          | TyBool, TyInt -> err ("Error: This expression has type int but an expression was expected of type bool"))
              | _ -> err("Syntax Error"))
  | LetExp (id, exp1, exp2) ->
        let tyarg1 = ty_exp tyenv exp1 in
            ty_exp (Environment.extend id tyarg1 tyenv) exp2
  | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented!")
