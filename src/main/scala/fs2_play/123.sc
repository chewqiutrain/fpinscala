import fs2._
import cats.effect.IO
import cats.implicits._

import scala.util.Try

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


println("-----------")

case class myIntException(intStr: String) extends Exception("This is world")
val mie1 = myIntException("-42")

val e1: Either[myIntException, Int] = Left(myIntException("-42"))
val e2: Either[myIntException, Int] = Left(myIntException("-99"))
val t1: Either[myIntException, Int] = Right(91)
val t2: Either[myIntException, Int] = Right(74)

val etl1: List[Either[myIntException, Int]] = List(t1, e1, t2, e2)

for {
  r1 <- t1
  l1 <- e1
  r2 <- t2
  l2 <- e2
} yield List(r1, l1, r2, l2)


def sequence(es: List[Either[myIntException, Int]]): Either[myIntException, List[Int]] = {
  def helper = (acc: Either[myIntException, List[Int]], elem: Either[myIntException, Int]) =>
    acc match {
      case Left(e) => Left(e)
      case Right(listA) => elem match {
        case Left(e) => Left(e)
        case Right(a) => Right(listA ++ List(a))
      }
    }

  es.foldLeft[Either[myIntException, List[Int]]](Right(List.empty[Int]))(helper)
}

sequence(etl1)


