open OUnit2
open Syntax
open List

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

let ex4_4 =
    let alpha = fresh_tyvar() in
    let beta = fresh_tyvar() in
    let a = Typing.unify [(TyFun (TyVar alpha, TyBool), TyFun (TyFun (TyInt, TyVar beta), TyVar beta))] in
    let b = Typing.unify [(TyVar alpha, TyVar beta)] in
    let test1 _ = assert_equal a ([(alpha, TyFun (TyInt, TyVar beta))] @ [(beta, TyBool)]) in
    let test2 _ = assert_equal b ([(alpha, TyVar beta)]) in
    "ex4_4">:::
    ["test1">:: test1;
     "test2">:: test2;]
;;

let check_type s =
    let exp, ans = s
    in (try
        let decl = Parser.toplevel Lexer.main (Lexing.from_string (exp ^ ";;")) in
        let s, ty = Typing.ty_decl Environment.empty decl in
        let pp_ty_test = pp_ty_string ty in
        let test _ = assert_equal pp_ty_test ans in
        test
        with Typing.Error s -> let test _ = assert_equal s ans in test
        | _ -> failwith "error")
;;

let ex4_6 =
    let test1 = check_type ("1 + 2", "int") in
    let test2 = check_type ("-2 * 2", "int") in
    let test3 = check_type ("1 < 2", "bool") in
    let test4 = check_type ("fun x -> x", "'a -> 'a") in
    let test5 = check_type ("fun x -> fun y -> x", "'a -> 'b -> 'a") in
    let test6 = check_type ("fun x -> fun y -> y", "'a -> 'b -> 'b") in
    let test7 = check_type ("(fun x -> x + 1) 2 + (fun x -> x + -1) 3", "int") in
    let test8 = check_type ("fun f -> fun g -> fun x -> g (f x)", "('a -> 'b) -> ('b -> 'c) -> 'a -> 'c") in
    let test9 = check_type ("fun x -> fun y -> fun z -> x z (y z)", "('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c") in
    let test10 = check_type ("fun x -> let y = x + 1 in x", "int -> int") in
    let test11 = check_type ("fun x -> let y = x + 1 in y", "int -> int") in
    let test12 = check_type ("fun b -> fun x -> if x b then x else (fun x -> b)", "bool -> (bool -> bool) -> bool -> bool") in
    let test13 = check_type ("fun x -> if true then x else (if x then true else false)", "bool -> bool") in
    let test14 = check_type ("fun x -> fun y -> if x then x else y", "bool -> bool -> bool") in
    let test15 = check_type ("fun n -> (fun x -> x (fun y -> y)) (fun f -> f n)", "'a -> 'a") in
    let test16 = check_type ("fun x -> fun y -> x y", "('a -> 'b) -> 'a -> 'b") in
    let test17 = check_type ("fun x -> fun y -> x (y x)", "('a -> 'b) -> (('a -> 'b) -> 'a) -> 'b") in
    let test18 = check_type ("fun x -> fun y -> x (y x) (y x)", "('a -> 'a -> 'b) -> (('a -> 'a -> 'b) -> 'a) -> 'b") in
    let test19 = check_type ("fun x -> fun y -> fun z -> x (z x) (y (z x y))", "((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> (((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> 'a) -> 'c") in
    let test20 = check_type ("let id = fun x -> x in let f = fun y -> id (y id) in f", "(('a -> 'a) -> 'a) -> 'a") in
    let test21 = check_type ("let k = fun x -> fun y -> x in let k1 = fun x -> fun y -> k (x k) in k1", "(('a -> 'b -> 'a) -> 'a) -> 'c -> 'b -> 'a") in
    let test22 = check_type ("let s = fun x -> fun y -> fun z -> x z (y z) in let s1 = fun x -> fun y -> fun z -> x s (z s) (y s (z s)) in s1", "((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e -> 'f) -> ((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e) -> ((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd) -> 'f") in
    let test23 = check_type ("let g = fun h -> fun t -> fun f -> fun x -> f h (t f x) in g", "'a -> (('a -> 'b -> 'c) -> 'd -> 'b) -> ('a -> 'b -> 'c) -> 'd -> 'c") in
    let test24 = check_type ("let s = fun x -> fun y -> fun z -> x z (y z) in let k = fun x -> fun y -> x in let k' = fun x -> fun y -> x in s k k';;", "'a -> 'a") in
    let test25 = check_type ("let s = fun x -> fun y -> fun z -> x z (y z) in let k = fun x -> fun y -> x in s k k", "Type Error") in
    let test26 = check_type ("let s = fun x -> fun y -> fun z -> x z (y z) in let k' = fun x -> fun y -> y in s k' k'", "Type Error") in
    let test27 = check_type ("fun x -> fun y -> fun z -> let b = x y z in if b then z y else y", "('a -> ('a -> 'a) -> bool) -> 'a -> ('a -> 'a) -> 'a") in
    let test28 = check_type ("let pair = fun x1 -> fun x2 -> fun y -> y x1 x2 in let proj1 = fun p -> p (fun x1 -> fun x2 -> x1) in let proj2 = fun p -> p (fun x1 -> fun x2 -> x2) in proj1 (pair 1 100)", "int") in
    let test29 = check_type ("let pair = fun x1 -> fun x2 -> fun y -> y x1 x2 in let proj1 = fun p -> p (fun x1 -> fun x2 -> x1) in let proj2 = fun p -> p (fun x1 -> fun x2 -> x2) in proj1 (proj2 (pair 10 (pair 20 30)))", "Type Error") in
    let test30 = check_type ("let f = fun x -> x in if f true then f 1 else f 2", "Type Error") in
    let test31 = check_type ("let f = fun x -> 3 in f true + f 4", "Type Error") in
    let test32 = check_type ("fun b -> let f = fun x -> x in let g = fun y -> y in if b then f g else g f", "Type Error") in
    let test33 = check_type ("fun b -> fun f -> let g1 = fun x -> x f in let g2 = fun x -> x f in fun z -> if b then g1 z g2 else g2 z g1", "Type Error") in
    let test34 = check_type ("1 + true", "Type Error") in
    let test35 = check_type ("2 + (fun x -> x)", "Type Error") in
    let test36 = check_type ("-2 * false", "Type Error") in
    let test37 = check_type ("fun x -> x x", "Type Error") in
    let test38 = check_type ("let f = fun x -> fun g -> g (x x g) in f f", "Type Error") in
    let test39 = check_type ("let g = fun f -> fun x -> f x (f x) in g", "Type Error") in
    let test40 = check_type ("let g = fun f -> fun x -> f x (x f) in g", "Type Error") in
    let test41 = check_type ("fun x -> fun y -> x y + y x", "Type Error") in
    let test42 = check_type ("fun x -> fun y -> x y + x", "Type Error") in
    let test43 = check_type ("fun x -> fun y -> if x y then x else y", "Type Error") in
    let test44 = check_type ("fun x -> fun y -> if x y then (fun z -> if y z then z else x) else (fun x -> x)", "Type Error") in
    let test45 = check_type ("fun x -> fun y -> fun z -> let b = x y z in if b then z y else z x", "Type Error") in
    let test46 = check_type ("fun x -> fun y -> fun z -> if x y then z x else y z", "Type Error") in
    let test47 = check_type ("fun x -> if x then 1 else x", "Type Error") in
    let test48 = check_type ("(fun x -> x + 1) true", "Type Error") in
    let test49 = check_type ("fun x -> fun y -> y (x (y x))", "Type Error") in
    let test50 = check_type ("(fun f -> fun x -> f (f x)) (fun x -> fun y -> x)", "Type Error") in
    let test51 = check_type ("fun x -> fun y -> y (x (fun z1 -> fun z2 -> z1)) (x (fun z -> z))", "Type Error") in
    let test52 = check_type ("fun b -> fun f -> let g1 = fun x -> f x in let g2 = fun x -> f x in if b then g1 g2 else g2 g1", "Type Error") in
    "ex4_6">:::
    ["test1">:: test1;
     "test2">:: test2;
     "test3">:: test3;
     "test4">:: test4;
     "test5">:: test5;
     "test6">:: test6;
     "test7">:: test7;
     "test8">:: test8;
     "test9">:: test9;
     "test10">:: test10;
     "test11">:: test11;
     "test12">:: test12;
     "test13">:: test13;
     "test14">:: test14;
     "test15">:: test15;
     "test16">:: test16;
     "test17">:: test17;
     "test18">:: test18;
     "test19">:: test19;
     "test20">:: test20;
     "test21">:: test21;
     "test22">:: test22;
     "test23">:: test23;
     "test24">:: test24;
     "test25">:: test25;
     "test26">:: test26;
     "test27">:: test27;
     "test28">:: test28;
     "test29">:: test29;
     "test30">:: test30;
     "test31">:: test31;
     "test32">:: test32;
     "test33">:: test33;
     "test34">:: test34;
     "test35">:: test35;
     "test36">:: test36;
     "test37">:: test37;
     "test38">:: test38;
     "test39">:: test39;
     "test40">:: test40;
     "test41">:: test41;
     "test42">:: test42;
     "test43">:: test43;
     "test44">:: test44;
     "test45">:: test45;
     "test46">:: test46;
     "test47">:: test47;
     "test48">:: test48;
     "test49">:: test49;
     "test50">:: test50;
     "test51">:: test51;
     "test52">:: test52]
;;

let () =
    run_test_tt_main ex4_1;
    run_test_tt_main ex4_2;
    run_test_tt_main ex4_3;
    run_test_tt_main ex4_4;

    run_test_tt_main ex4_6;
    (* run_test_tt_main (check_type ("", "")); *)

;;