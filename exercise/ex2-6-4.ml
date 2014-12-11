(* 比較演算子で'a'から'z'の区間に入るかチェックする。
 * 入るならcharをintに変換してから計算を実行する。
 * 入らない場合はそのまま返す。
 *
 * 実行例
 * # capitalize 'z';;
 * - : char = 'Z'
 * # capitalize '-';;
 * - : char = '-'
 * *)
let capitalize c =
    if (c >= 'a') && (c <= 'z') then
        char_of_int (int_of_char c - int_of_char 'a' + int_of_char 'A')
    else
        c
;;

