import java.nio.file.Path

import fs2._
import cats.effect.IO

val x: IO[Int] = IO{42}

x.unsafeRunSync()

val currentTime: IO[Long] = IO{System.currentTimeMillis()}

//.eval is saying for the next element in the stream, evaluate
// def eval[F[_], O](fo: F[O]): Stream[F, O] =
//  evaluate F[O], and for any values that come out of that evaluation,
//  make that the output of the stream
val y: Stream[IO, Long] = Stream.eval(currentTime)
//val y2 = y.toList // we can't call this because it is not pure.

// compile.toVector returns IO[Vector[Long]]. This says get all the values
//  in a vector and return the vector inside our effect type (in this case, IO)
val y1 = y.compile.toVector.unsafeRunSync()

//compile.toVector is dangerous because if the stream is an infinite stream
// then it will never return because it keeps accumulating elements
// in the vector
val y2 = y.repeat.take(5).compile.toVector.unsafeRunSync()
val y2a = Stream.repeatEval(currentTime).take(5).compile.toVector.unsafeRunSync()
// y2 and y2a are equivalent. repeatEval is just an alias.
//~/Documents/GitHub/fpinscala/src/main/resources/fahrenheit.txt
val fd1 = java.nio.file.Paths.get(System.getProperty("user.home"), "Documents", "GitHub", "fpinscala", "src", "main", "resources", "fahrenheit.txt")
val src: Stream[IO, Byte] = io.file.readAll[IO](fd1, 16)
src.compile.toVector.unsafeRunSync()

//val byte2strPipe: Pipe[Nothing, Byte, String] = text.utf8Decode
val z1: Stream[IO, String] = text.utf8Decode(src)
val z1a = z1.compile.toVector.unsafeRunSync()

//a more idiomatic method..
val z1b: Stream[IO, String] = src.through(text.utf8Decode)
val z1bb: Vector[String] = z1b.compile.toVector.unsafeRunSync()

// notice how the number of elements in the vector is less than 105, which is
// the number of values in the file. this is because we specified to read
// 16 bytes at a time. what if we want to reconstitute and separate it out
// by lines?
z1bb.size
z1bb(10)

val z2 = text.lines
val z2a = z1.through(text.lines)
val z2aa = z2a.compile.toVector.unsafeRunSync()
// fs2 will manage resource allocation. This includes file handles

def fahrenheitToCelsius(f: Double): Double = {
  (f - 32.0) * (5.0/9.0)
}


//remove invalid values and convert to celsius values
val z3a: Stream[IO, String] = z2a.filter(s => !s.trim.isEmpty && !s.startsWith("//"))
    .map(_.toDouble).map(fahrenheitToCelsius).map(_.toString)

//now we want to write to disk. We need to intersperse newline chars
val z3b: Stream[IO, String] = z3a.intersperse("\n")

//val z3c = z3b.compile.toVector.unsafeRunSync()

val outfd1: Path = java.nio.file.Paths.get(System.getProperty("user.home"), "Documents", "GitHub", "fpinscala", "src", "main", "resources", "celsius.txt")

// Sink type is just Pipe from something to Unit (nothing) with some effect
//  type Sink[F[_], -I] = Pipe[F, I, Unit]
val outpipe: Sink[IO, Byte] = io.file.writeAll[IO](fd1)

val preOutPipe: Stream[IO, Byte] = z3b.through(text.utf8Encode)

val finallyCanWrite: Stream[IO, Unit] = preOutPipe.through(outpipe)

// we dont want to compile.toVector because we don't care about the result
// we only care that the results were written to disk. i.e. that the effects
// are executed.
val almostThere: IO[Unit] = finallyCanWrite.compile.drain
//almostThere.unsafeRunSync()


