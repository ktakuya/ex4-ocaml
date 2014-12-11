(* iの中身が0ではない間ループを回す。
 * ループ内ではres自身にiをかけ、iを１つずつ減らす。
 * 
 * 実行例
 * # fact_imp 5;;
 * - : int = 120
 * *)
let fact_imp n =
    let i = ref n and res = ref 1 in
        while (!i != 0) do
            res := !res * !i;
            i := !i - 1
        done;
        !res
;;

