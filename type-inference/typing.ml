open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

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

(*
  subst_type : subst(aka (tyvar * ty) list) -> ty -> ty
*)
let rec subst_type s ty =
    let rec resolve_type s = function
        TyVar (var) ->
          (let i, v = s in
           if i = var then v else (TyVar var))
      | TyFun (ty1, ty2) -> (TyFun ((resolve_type s ty1), (resolve_type s ty2)))
      | a -> a in
    match s with
      [] -> ty
    | hd :: tl -> subst_type tl (resolve_type hd ty)

(*
  subst_ty_list : subst -> (ty * ty) list -> (ty * ty) list
*)
let rec subst_ty_list s = function
    [] -> []
  | (ty1, ty2)::r -> ((subst_type s ty1), (subst_type s ty2)) :: subst_ty_list s r
;;

(*
  unify : (ty * ty) list -> subst(aka (tyvar * ty) list)
*)
let rec unify = function
    [] -> []
  | (TyInt, TyInt)::r | (TyBool, TyBool)::r -> unify r
  | (TyVar v1, TyVar v2)::r ->
      (if v1 = v2 then
        unify r
      else
        (v1, TyVar v2)::unify (subst_ty_list [(v1, TyVar v2)] r))
  | (TyVar v, ty)::r | (ty, TyVar v)::r ->
      (if MySet.member v (freevar_ty ty) then
        err ("Type Error")
      else
        (v, ty) :: unify (subst_ty_list [(v, ty)] r))
  | (TyFun (ty11, ty12), TyFun (ty21, ty22))::r ->
      unify ((ty11, ty21) :: (ty12, ty22) :: r)
  | _ -> err ("Type Error")
;;
