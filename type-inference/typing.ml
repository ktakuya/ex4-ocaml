open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

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
  型の等式集合に型代入を適用
 *)
let rec subst_ty_list s = function
    [] -> []
  | (ty1, ty2)::r -> ((subst_type s ty1), (subst_type s ty2)) :: subst_ty_list s r
;;

(*
  eqs_of_subst : subst -> (ty * ty) list
  型代入を型の等式集合に変換
 *)
let eqs_of_subst s =
  List.map (fun (v, ty1) -> (TyVar v, ty1)) s
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

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | LogAnd -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | LogOr -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | _ -> err "Not Implemented!"

let rec ty_exp tyenv = function
    Var x ->
        (try ([], Environment.lookup x tyenv) with
            Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
        let (s1, ty1) = ty_exp tyenv exp1 in
        let (s2, ty2) = ty_exp tyenv exp2 in
        let (eqs3, ty) = ty_prim op ty1 ty2 in
        let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
        let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) -> 
        let (scond, tycond) = ty_exp tyenv exp1 in
        let (s1, ty1) = ty_exp tyenv exp2 in
        let (s2, ty2) = ty_exp tyenv exp3 in
        let eqs = [(tycond, TyBool)] @ (eqs_of_subst scond) @ (eqs_of_subst s1) @
                    (eqs_of_subst s2) @ [(ty1, ty2)] in
        let s3 = unify eqs in (s3, subst_type s3 ty1)
  | LetExp (id, exp1, exp2) ->
        let (s1, ty1) = ty_exp tyenv exp1 in
        let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
        let domty = TyVar (fresh_tyvar ()) in
        let eqs = [(domty, ty1)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
        let s3 = unify eqs in (s3, subst_type s3 ty2)
  | FunExp (id, exp) ->
        let domty = TyVar (fresh_tyvar ()) in
        let s, ranty = ty_exp (Environment.extend id domty tyenv) exp in
          (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
        let (s1, ty1) = ty_exp tyenv exp1 in
        let (s2, ty2) = ty_exp tyenv exp2 in
        let domty = TyVar (fresh_tyvar ()) in
        let eqs = [(ty1, TyFun (ty2, domty))] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
        let s3 = unify eqs in (s3, subst_type s3 domty)
  | _ -> err ("Not Implemented!")


let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented!")


