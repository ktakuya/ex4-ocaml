(* 反復式に計算していく 
 *
 * 実行例
 * # fib_iter 70;;
 * - : int = 190392490709135
 * *)
let fib_iter n =
    let rec fibi a b n =
        if n = 0 then
            b
        else
            fibi (a + b) a (n - 1)
    in fibi 1 0 n
;;

