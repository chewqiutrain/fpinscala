import fs2._
import cats.effect.IO
import cats.implicits._

def nestedIOs(x: Int): IO[String] = IO {
  println("Outer IO")
  x.toString
}

//nestedIOs(42).unsafeRunSync()


val l1 = List("apple1", "banana")
val l2 = List("coconut", "dick")
val x: Stream[IO, List[String]] = Stream(l1, l2)
x.compile.toVector.unsafeRunSync().flatten


val l: List[Either[String, Int]] = List(Right(1), Left("error"), Left("Hello"), Right(42), Left("piggy"))
l.separate


val l3 = l1 :: l2
