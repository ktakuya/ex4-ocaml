(* gは関数で返る値も関数であるから
 * Left gの場合は引数をaとしてgにaを適用させたものをLeftで返す
 * Rightは先と同様。
 *
 * 実行例
 * # (f (Left (fun x -> x + 1))) 2;;
 * - : (int, 'a) sum = Left 3
 * *)
type ('a, 'b) sum = Left of 'a | Right of 'b;;

let f = function
    Left g -> (fun a -> (Left (g a)))
    | Right g -> (fun a -> (Right (g a)))
;;

