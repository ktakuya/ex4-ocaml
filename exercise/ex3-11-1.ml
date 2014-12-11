(* nが0ならmを返し
 * Euclidの互除法に従い
 * nとm / nの剰余の最大公約数を返す
 *
 * 実行例
 * # gcd 6 2;;
 * - : int = 2
 * # gcd 624129 2061517;;
 * - : int = 18913
 * *)
let rec gcd m n =
    if n = 0 then
        m
    else
        gcd n (m mod n)
;;

