(* どちらかのリストが空になるまで
 * 先頭同士をタプルにしてconsでつなげていく。
 *
 * 実行例
 * # zip [1;2;3] [4;5;6];;
 * - : (int * int) list = [(1, 4); (2, 5); (3, 6)]
 * # zip [1;2;3] [4];;
 * - : (int * int) list = [(1, 4)]
 * *)
let rec zip a b =
    if List.length a = 0 || List.length b = 0 then
        []
    else
        (List.hd a, List.hd b) :: zip (List.tl a) (List.tl b)
;;

