(* nが0であれば1.0を返す。
 * まずn/2でpowを計算する。
 * nが偶数の場合は先に計算した値同士を掛けあう。
 * nが奇数の場合は先に計算した値同士とxを掛けあう。
 *
 * 実行例
 * # pow 3.0 5;;
 * - : float = 243.
 * *)
let rec pow x n =
    if n = 0 then
        1.0
    else
        let powd = pow x (n / 2) in
        if n mod 2 = 0 then
            powd *. powd
        else
            x *. powd *. powd
;;

