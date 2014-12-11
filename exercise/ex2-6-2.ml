(* 10倍しておいて桁上げしておいて
 * １セント以下はfloorを使って四捨五入する
 *
 * 実行例
 * ange_yen_to_us 100;;
 * - : float = 0.9
 * # change_yen_to_us 132;;
 * - : float = 1.2
 * *)

let change_yen_to_us n = 
    floor ((float_of_int n) *. 10.0 /. 111.12 +. 0.5) /. 10.0
;;

