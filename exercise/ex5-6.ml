(* quickからの変更点は分割統治で再帰的に処理していく際に
 * 大きな値からsortedにconsでつなげていくようにした点。
 * 
 * 実行例
 * # quicker [4;2;6;2;1;3;9] [];;
 * - : int list = [1; 2; 2; 3; 4; 6; 9]
 * *)
let rec quicker l sorted =
    match l with
    [] -> sorted
    | [x] -> x :: sorted
    | x :: xs -> 
            let rec partition left right = function
                [] -> quicker left (x::(quicker right sorted))
                | y :: ys -> if x < y then partition left (y :: right) ys
                             else partition (y :: left) right ys
            in partition [] [] xs
;;

