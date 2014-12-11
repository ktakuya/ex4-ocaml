(* changeの引数が最後のパターンとマッチングする場合、
 * 一つ前でtotalから引き過ぎているので
 * failwith "change"で例外を発生させてバックトラックする。
 * その処理はwith以下にてcを使わずに残りのコインでtotalを計算する。
 * 
 * 実行例
 * # change (gb_coins, 43);;
 * - : int list = [20; 20; 2; 1]
 * # change (us_coins, 43);;
 * - : int list = [25; 10; 5; 1; 1; 1]
 * # change ([5; 2], 16);;
 * - : int list = [5; 5; 2; 2; 2]
 * *)
let rec change = function
    (_, 0) -> []
    | ((c :: rest) as coins, total) ->
            if c > total then
                change (rest, total)
            else
                (try
                    c :: change (coins, total - c)
                with Failure "change" ->
                    change (rest, total)
                )
    | _ -> failwith "change";;

let us_coins = [25; 10; 5; 1] and gb_coins = [50; 20; 10; 5; 2; 1];;

