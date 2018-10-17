import fs2.Pipe.Stepper.Emits
import fs2._

val x1: Stream[Pure, Int] = Stream(1,2,3) // initialize a stream with these elements

val x1a = x1.map(_ + 1) // normal map operation over a collection

val x1b = x1.flatMap(v => Stream.emits(List.fill(v)(v)))


val x2 = Stream.emits(Seq(1,2,3)) // equivalent to x1

// since effect type is Pure, we can just all toVector or toList, etc.
x1.toVector
x1a.toVector
x1b.toVector

x2.toVector
x2.toList

val x3 = Stream(4,5,6)
x1.interleave(x3).toList // alternate elements from x1 and x3

val x4 = Stream.range(0,10)
x4.toList
x4.intersperse(-42).toList // -42 is filled at every alternate index

x4.zip(x1).toList // zip terminates when it reaches the end of the shorter stream

val x1repeated: Stream[Pure, Int] = x1.repeat // infinite stream

x4.zip(x1repeated).toList // zip now ends because x4 is shorter

x1repeated.take(7).toList // only take the first 7 elements from the stream





