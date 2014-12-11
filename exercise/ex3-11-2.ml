(* combinationの定義通り 
 *
 * 実行例
 * # comb 7 2;;
 * - : int = 21
 * *)
let rec comb n m =
    if (m = 0) || (n = m) then
        1
    else
        (comb (n - 1) m) + (comb (n - 1) (m - 1))
;;

