(* nが0ならすべて置換できたので完了
 * それ以外なら与えられた定義からn以下の値の組を見つける。
 * その組に対応した文字に置換しつつnから値を引く、
 * もし現在の先頭の組でn以下の値を持つ組がなければ
 * 次の組を探索する。
 * また以下は組の先頭が大きい順に定義が与えられると仮定している。
 *
 * 実行例
 * # roman [(1000,"M");(500,"D");(100,"C");(50,"L");(10,"X");(5,"V");(1,"I")] 1984;;
 * - : string = "MDCCCCLXXXIIII"
 * # roman [(1000,"M");(900,"CM");(500,"D");(400,"CD");(100,"C");(90,"XC");(50,"L");(40,"XL");(10,"X");(9,"IX");(5,"V");(4,"IV");(1,"I")] 1984;;
 * - : string = "MCMLXXXIV"
 * *)
let rec roman def n =
    let head = List.hd def in
    let tail = List.tl def in
    if n = 0 then
        ""
    else
        match head with
        (x,y) -> if x <= n then
                    y ^ roman def (n - x)
                 else
                    roman tail n
;;

