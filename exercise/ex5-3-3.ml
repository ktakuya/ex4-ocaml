(* 先頭のリストと後方のリストをappendしていく。
 *
 * 実行例
 * # concat [[0; 3; 4]; [2]; [5; 0]; []];;
 * - : int list = [0; 3; 4; 2; 5; 0]
 * *)
let rec concat l =
    match l with
    [] -> [] 
    | hd :: tl ->
         hd @ concat tl
;;

