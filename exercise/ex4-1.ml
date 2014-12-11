(* 区間の幅を100000としてdeltaの値を決めておいて
 * 再帰的にループさせて定義通りに積分値を計算する
 *
 * 実行例
 * # integral (fun x -> x) 2.0 4.0;;
 * - : float = 5.99999999999999911
 * *)
let integral f a b =
    let n = 100000 in
    let delta = (b -. a) /. float_of_int n in
    let rec trapezoid i =
        if i = n + 1 then
            0.0
        else
            (f (a +. float_of_int (i - 1) *. delta) +. f (a +. float_of_int i *. delta)) *. delta /. 2.0 +. 
            trapezoid (i + 1) in
    trapezoid 1
;;

(*
 * sinの0からpiまで積分はsinpiとして定義した
 * *)

let sinpi =
    let pi = 3.14159265 in
    integral sin 0.0 pi
;;

