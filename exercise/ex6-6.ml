(* reflectはrootから右側と左側の子を逆転して木を構成する。
 *
 * 実行例
 * 
 * # let comptree = Br(1, Br(2, Br(4, Lf, Lf), Br(5, Lf, Lf)), Br(3, Br(6, Lf, Lf), Br(7, Lf, Lf)));;
 * val comptree : int tree = Br (1, Br (2, Br (4, Lf, Lf), Br (5, Lf, Lf)), Br (3, Br (6, Lf, Lf), Br (7, Lf, Lf)))
 * # reflect comptree;;
 * - : int tree = Br (1, Br (3, Br (7, Lf, Lf), Br (6, Lf, Lf)), Br (2, Br (5, Lf, Lf), Br (4, Lf, Lf)))
 * *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec reflect = function
    Lf -> Lf
    | Br (v, left, right) -> Br (v, reflect right, reflect left)
;;

let comptree = Br(1, Br(2, Br(4, Lf, Lf),
Br(5, Lf, Lf)),
Br(3, Br(6, Lf, Lf),
Br(7, Lf, Lf)));;

(*
 * reverse(l) はリストを逆順にする関数とする。
 * preorder(reflect(t)) = reverse(postrder(t))
 * inorder(reflect(t)) = reverse(inorder(t))
 * postorder(reflect(t)) = reverse(preorder(t))
 * *)

