(* nが0なら[0]
 * それ以外なら再帰的に先頭にnを付け加えていく
 *
 * 実行例
 * # downto0 5;;
 * - : int list = [5; 4; 3; 2; 1; 0]
 * *)
let rec downto0 n =
    if n = 0 then
        [0]
    else
        n :: downto0 (n - 1)
;;

