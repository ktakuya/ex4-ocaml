(* nが負数になるまではxとpow x n-1を掛けて再帰的に
 * 呼び出す。
 *
 * 実行例
 * # pow 3.0 5;;
 * - : float = 243.
 * *)
let rec pow x n =
    if n = 0 then
        1.0
    else
        x *. pow x (n-1)
;;

