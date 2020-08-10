# dep-monad
Scala - Dependency injection monad - experiments.

It would satisfy dependencies in the for comprehension during the compilation. 

```
  val dep = for {
    len <- reader{(str: String, str2: B1) => Dep2B1(str.length) }
    len2 = len.i*2
    _ <- reader{b1: B1 => B2(b1.i)}
    _ <- reader{(sameStr4: String, b2: B2, b3: B3) => B5(5*2 + sameStr4.length + b3.i) }
    fn <- reader{ f: Final => f.i * 2L}
  } yield AnotherFinal(len2+fn.toInt)
```

All types in the for comprehension would satisfy requirements in all underlying for comprehension lines.
There should be one unique type produced on each line. 
