(* a'とLeftのタプルならa'とその値をタプルにしてLeftで返す
 * a'とRightのタプルならa'とその値をタプルにしてRightで返す
 *
 * 実行例
 * # f (3, Left 1);;
 * - : (int * int, int * 'a) sum = Left (3, 1)
 * # f (3, Right 2);;
 * - : (int * 'a, int * int) sum = Right (3, 2)
 * *)
type ('a, 'b) sum = Left of 'a | Right of 'b;;

let f = function
    (a', Left b') -> (Left (a',b'))
    | (a', Right c') -> (Right (a', c'))
;;


