import scala.{Stream => _ }
import fpinscala.laziness.Stream


val x = Stream(1,2,3,4,5)

val taker: Stream[Int] = x.take(2)

val taker2: Stream[Int] = x.take(2)


//val infs: Stream[Int] = Stream.fibs()
//
//val takes = infs.take(6).toList
//println(takes)

val infs = Stream.constant(1)
val takes = infs.take(6).toList

println(takes)

val maybeOnesF: Int => Some[(Int, Int)] = x => Some(1,1)
maybeOnesF(1)



//def fibUnfoldHelper1: (Int)(Int) => Option[(Int, Int)] = (n1: Int, n2: Int) => Some(n2, n1 + n2)

def curried(n1: Int)(n2: Int): (Int, Int) = (n1, n1 + n2)
val f1: Int => (Int, Int) = curried(1)
f1(12)
f1(13)
f1(10)


val x4 = Some(1 -> 2)
