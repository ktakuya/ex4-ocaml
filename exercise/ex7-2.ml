(* 参照が与えられるので!xで中身を取り出して1加算し代入する。
 *
 * 実行例
 * # let x = ref 3;;
 * val x : int ref = {contents = 3}
 * # incr x;;
 * - : unit = ()
 * # !x;;
 * - : int = 4
 * *)
let incr x =
    x := !x + 1
;;

