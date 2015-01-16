open OUnit2
open Syntax

let ex4_1 =
    let x = 3 in
    let x_3 _ = assert_equal x 3 in
    "ex4_1">:::
    ["x_3">::x_3]
;;

let ex4_2 =
    let a = fresh_tyvar() in
    let b = fresh_tyvar() in
    let test1 _ = assert_equal a 0 in
    let test2 _ = assert_equal b 1 in
    let x = TyFun ((TyVar a), (TyFun (TyVar b, TyVar a))) in
    let test3_freevar _ = assert_equal (freevar_ty x) (MySet.insert a (MySet.insert b (MySet.singleton a))) in
    "ex4_2">:::
    ["test1">:: test1;
     "test2">:: test2;
     "test3_freevar">:: test3_freevar;]
;;

let ex4_3 =
    let alpha = fresh_tyvar() in
    let beta = fresh_tyvar() in
    let x = Typing.subst_type [(alpha, TyInt)] (TyFun (TyVar alpha, TyBool)) in
    let y = Typing.subst_type [(beta, TyFun (TyVar alpha, TyInt)); (alpha, TyBool)] (TyVar alpha) in
    let z = Typing.subst_type [(beta, TyFun (TyVar alpha, TyInt)); (alpha, TyBool)] (TyVar beta) in
    let test1 _ = assert_equal x (TyFun (TyInt, TyBool)) in
    let test2 _ = assert_equal y (TyBool) in
    let test3 _ = assert_equal z (TyFun (TyBool, TyInt)) in
    "ex4_3">:::
    ["test1">:: test1;
     "test2">:: test2;
     "test3">:: test3;]
;;

let _ =
    run_test_tt_main ex4_1;
    run_test_tt_main ex4_2;
    run_test_tt_main ex4_3;
;;