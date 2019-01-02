case class MyClass(i: Int)
type Mine = Either[Throwable, MyClass]
type OuterMine = Either[Throwable, Int]

val ok1: Mine = Right(MyClass(111))
val notOk1: Mine = Left(new Exception("exception"))

val f: Mine => OuterMine = (in: Mine) => in.flatMap{
  x => Right(x.i)
}

f(ok1)
f(notOk1)





