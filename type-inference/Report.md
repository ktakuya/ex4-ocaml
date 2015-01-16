# Exercise 4.1(必修課題)
syntax.mlに型推論のための型を定義し、main.mlでは処理を実行する前に型推論を行うようにeval_declの前にty_declを追加する。  
typing.mlではty_primでプリミティブ型の処理の型推論を行う、規則は資料通りに行う、２つの引数から判定し、もし規則通りでなければエラーを返す。  
ty_expでは型推論用の環境変数を受け取って、推論する型によって処理を分ける。IfExpでは条件式がBoolである必要があり、またthen節とelse節の推論結果から型が正しいか判定するように実装し、値によってエラーの内容を変えた。  
LetExpはexp1の型推論結果をidに紐付けて、環境を拡張し、exp2の型推論結果を返す。  

```
$ ./miniml
# if true then 1 else 2;;
val - : int = 1
# let a = 3 in a + 4;;
val - : int = 7
```

# Exercise 4.2(必修課題)
# Exercise 4.3(必修課題)
# Exercise 4.4(必修課題)
# Exercise 4.5(必修課題)
例えば、 `(TyVar 'a', TyFun (TyBool, TyVar 'a'))` について考えた時、'a'にどのような型を割り当てたとしても一致することはないため。
# Exercise 4.6(必修課題)
