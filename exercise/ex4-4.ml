(* カリー化された関数を受け取るので
 * 右辺はx, yの順に適用していく。
 * また左辺のfの引数には２つ組を受け取る。
 *
 * 実行例
 * # let cf = fun x y -> x + y;;
 * val cf : int -> int -> int = <fun>
 * # cf 1 2;;
 * - : int = 3
 * # let ucf = uncurry cf;;
 * val ucf : int * int -> int = <fun>
 * # ucf (1, 2);;
 * - : int = 3
 * *)
let uncurry f (x, y) =
    (f x) y
;;

