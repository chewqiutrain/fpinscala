import cats.effect.IO
import cats.implicits._

//val x: IO[Int] = IO{1}
//val y: IO[Unit] = x *> IO{println("hello")}
//y.unsafeRunSync()

import cats.effect.{IO, Async}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure}

val apiCall: Future[String] = Future.successful("I come from the Future!")

val ioa: IO[String] =
  Async[IO].async { cb =>
    val y: Either[Throwable, String] => Unit = cb
    import scala.util.{Failure, Success}
    val x: Unit = apiCall.onComplete {
      case Success(value) => cb(Right(value)) // why is this not infinitely recursive?
      case Failure(error) => cb(Left(error))
    }
    x
  }

ioa.unsafeRunSync()


import scala.concurrent.ExecutionContext.global

//type Async[A] = (A => Unit) => Unit
//
//def timesTwo(n: Int): Async[Int] =
//  onFinish => {
//    val y: Int => Unit = onFinish
//    global.execute(new Runnable {
//      def run(): Unit = {
//        val result = n * 2
//        onFinish(result)
//      }
//    })
//  }
//
// Usage
//timesTwo(20) { result => println(s"Result: $result") }
//=> Result: 40

def m(f: Int => Unit): Unit = f(24)

m
