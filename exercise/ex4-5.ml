(* let (fibn, _) から組を使うことがわかる。
 * あとは反復する計算式をrepeatに使う。
 *
 * 実行例
 * # fib 70;;
 * - : int = 190392490709135
 * *)
let rec repeat f n x =
    if n > 0 then
        repeat f (n - 1) (f x)
    else
        x
;;

let fib n =
    let (fibn, _) = 
       repeat (fun (a, b) -> (b, a + b)) n (0, 1) 
    in fibn
;;

