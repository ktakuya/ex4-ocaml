(* max_ascii_rec の引数maxは文字列中のn番目までにおける
 * 最大の文字を表す。
 * あとは、max_ascii_recを最小の文字とインデックスを0から呼び出し、
 * 再帰的に解析していく文字を繰り上げていくことで
 * ASCIIコードが最も大きい文字がmaxに格納される。
 *
 * 実行例
 * # max_ascii "afaiowe-=12;|`~;";;
 * - : char = '~'
 * *)
let max_ascii s =
    let rec max_ascii_rec max n =
        if n = (String.length s) then
            max
        else
            if s.[n] > max then
                max_ascii_rec s.[n] (n + 1)
            else
                max_ascii_rec max (n + 1)
    in max_ascii_rec (char_of_int 0) 0 
;;

