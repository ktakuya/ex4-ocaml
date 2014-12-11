(* siftはxがnで割り切れたら次のxを試す。
 * 割り切れなければxをConsで追加して次のxを試す。
 *
 * 実行例
 * aは3の倍数を除いた列を返す。
 * # let a = sift 3 (from 1);;
 * val a : int seq = Cons (1, <fun>)
 * # take 10 a;;
 * - : int list = [1; 2; 4; 5; 7; 8; 10; 11; 13; 14]
 * *)
type 'a seq = Cons of 'a * (unit -> 'a seq);;
let rec from n = Cons (n, fun () -> from (n + 1));;
let head (Cons (x, _)) = x;;
let tail (Cons (_, f)) = f ();;
let rec take n s =
    if n = 0 then 
        [] 
    else head s :: take (n - 1) (tail s);;

let rec mapseq f (Cons (x, tail)) =
    Cons (f x, fun () -> mapseq f (tail ()));;


let rec sift n (Cons (x, tail)) =
    if x mod n = 0 then
        sift n (tail ())
    else
        Cons (x, fun () -> sift n (tail ()))
;;

let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f())));;
let primes = sieve (from 2);;

let rec nthseq n (Cons (x, f)) =
    if n = 1 then x else nthseq (n - 1) (f());;
(*
 * 出席番号+3000番目の素数は以下を実行することで31319だとわかる。
 * # nthseq 3375 primes;;
 * - : int = 31319
 *)

