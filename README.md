# lc19-talk
Dotty code examples for "What can FP learn from Grammars?"

Code explanation and examples in [AlgebraicApplication.scala](./src/main/scala/quaternion/algae/AlgebraicApplication.scala)

```
$ sbt console
scala> val intCode: RFun1[Int, String] = (i:Int) => "int $i"
scala> val x = RD3[Int, Boolean, Double](false)

scala> val y: String | Boolean | Double = x |> intCode
 
```


