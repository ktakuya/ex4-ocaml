# Exercise 3.1(必修課題)
* ML1 インタプリタのプログラムをコンパイル・実行し，インタプ
リタの動作を確かめよ.   

```
$ make depend
$ make
$ ./miniml
# 3 + 3;;
val - = 6
# 4 * -1;;
val - -4
```

* 大域環境として i, v, x の値のみが定義されているが，ii が 2，iii が3，iv が 4 となるようにプログラムを変更して，動作を確かめよ.  

Main.mlのinitial_envにてEnvironment.extendでii, iii, ivをそれぞれInt型の2, 3, 4に拡張する。  
 
```
$ ./miniml
# iv + iii * ii;;
val - = 10
```


## Exercise 3.3(⋆)
* 論理値演算のための二項演算子 &&, || を追加せよ．

変更を加えたファイルは,lexer.mll, parser.mly, syntax.ml, eval.ml   
まずlexer.mllには、&&をLOGAND, ||をLOGORとして追加する。  
parser.mlyには構文としてLOGExprの構文を追加、それぞれBinOpを生成する。  
syntax.mlにはbinOpの型としてLogAndとLogOrを追加する。  
eval.mlではapply_primのパターンマッチにLogAndとLogOrを追加する。また２つの引数はともにブーリアン型の必要がある。  


## Exercise 3.4(必修課題)
* 実行例  

```
$ ./miniml
# let a = 3 * 3;;
val a = 9
# let b = 5 in b * 3;;
val - = 15
# b;;
Variable not bound: b
```
１つ目はlet宣言でaに9を割り当てた例、２つ目はlet式でbに5を割り当てて式内でb*3を評価した例である。  
３つ目の入力でbは確かに式内でのみ有効であることがわかる。
## Exercise 3.8(必修課題)
* 実行例

```
$ ./miniml
# let x = 2 in let addx = fun y -> x + y in addx 4;;
val - = 6
# let f = fun x -> x * x in let g = fun f -> fun x -> f x in g f 3;;
val - = 9
```

１つ目の入力ではまずxに2を割り当て、関数addxでは１つの引数を取りその引数と先に宣言したxの和を返す。結果として4+2が評価されて6が返っている。  
２つ目の入力では、関数fは引数の累乗を返し、gは２つの引数を取り２つ目の引数を１つ目の引数の関数に適用する。結果として`g f 3`は3の累乗を返し、確かに高階関数が正しく動作している。  
## Exercise 3.9(⋆⋆)
中置演算子用の構文をparser.mlyに追加し、eval.mlにInfixExp用のパターンマッチを追加する。  
InfixExpにマッチした時、それぞれの演算子は`fun x -> fun y -> x op y`の構文木を返す。  
- 実行例 

```
$ ./miniml
# let threetimes = fun f -> fun x -> f (f x x) (f x x) in threetimes (+) 5;;
val - = 20
# let f = (+) in f 3 5;;
val - = 8
```

## Exercise 3.10(⋆)
`fun x1 ... xn -> ...`のサポートのためにfunの引数を複数個持たせるようにparser.mlyを変更した。引数部分を解析する構文をFunExprArgumentListとし、この構文を繰り返し引数分だけ消費する。  
`let f x1 ... xn -= ...`のサポートのために先と同じような構文をparser.mlyに追加した。こちらもLetExprArgumentListとして出現したIDをFunExpの第一引数としてつなげていく。  
- 実行例

```
$ ./miniml
# (fun x y z -> x * y * z) 1 2 3;;
val - = 6
# let f x y z = x + y + z;;
val f = fun
# f 1 2 3;;
val - = 6
```

`fun x y z -> x * y * z` は `FunExp(x, FunExp(y, FunExp(z, Expr)))` といった構文木を生成する。  
`let f x y z = x + y + z` は `Decl(f, FunExp(x, FunExp(y, FunExp(z, Expr))))` といった構文木を生成する。
## Exercise 3.11(⋆)
階乗を計算するプログラムは以下である。

```
# let makemult = fun maker -> fun x -> fun n -> if x < 1 then 0 else n + maker maker (x + -1) n;;
# let timesn = fun x -> fun n -> makemult makemult x n;;
# let rec fact = fun n -> if n < 2 then 1 else timesn (fact (n+-1)) n in fact 5;;  
val - = 120
```

timesnは２つの引数をとって`x * n`を計算する。これを使って、`fact(1) * fact(2) * ...`を計算する。
## Exercise 3.14(必修課題)

