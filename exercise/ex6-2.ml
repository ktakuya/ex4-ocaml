(* int_of_nat はListの長さを求める方法と同じ要領で計算する。
 * mul はmを一つずつ減らしつつ、nを加えていく。
 * monus はmとnを同時に減らしつつ、どちらかがZeroになればその時のmを返す。
 *
 * 実行例
 * # let three = OneMoreThan (OneMoreThan (OneMoreThan Zero));;
 * val three : nat = OneMoreThan (OneMoreThan (OneMoreThan Zero))
 * # let two = OneMoreThan (OneMoreThan Zero);;
 * val two : nat = OneMoreThan (OneMoreThan Zero)
 * # mul three two;;
 * - : nat = OneMoreThan (OneMoreThan (OneMoreThan (OneMoreThan (OneMoreThan (OneMoreThan Zero)))))
 * # monus three two;;
 * - : nat = OneMoreThan Zero
 * # monus two three;;
 * - : nat = Zero
 * *)
type nat = Zero | OneMoreThan of nat;;

let rec int_of_nat = function
    Zero -> 0
    | OneMoreThan n' -> 1 + int_of_nat n'
;;

let rec add m n =
    match m with
    Zero -> n
    | OneMoreThan m' -> OneMoreThan (add m' n)
;;

let rec mul m n =
    match m with
    Zero -> Zero
    | OneMoreThan m' -> add n (mul m' n)
;;

let rec monus m n =
    match m with
    Zero -> Zero
    | OneMoreThan m' -> match n with
                        Zero -> m
                        | OneMoreThan n' -> monus m' n'
;;

