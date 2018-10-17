import fs2._
import cats.effect.IO
import fs2.async.mutable.Signal

import scala.concurrent.duration.FiniteDuration

val src: Stream[Pure, Int] = Stream.range(0,100).take(3)
src.toList


//def myTake[F[_], A](n: Long): Pipe[F, A, A] = {
//  def go(s: Stream[F, A], n: Long): Pull[F, A, Unit] = {
//    s.pull.uncons.flatMap(x =>
//      x match {
//        case Some((hd, tl)) => hd
//      }
//    )
//  }
//  Segment
//}

//def tk[F[_],O](n: Long): Pipe[F,O,O] = {
//  def go(s: Stream[F,O], n: Long): Pull[F,O,Unit] = {
//    s.pull.uncons.flatMap {
//      case Some((hd,tl)) =>
//        hd.size match {
//          case m if m <= n => Pull.output(hd) >> go(tl, n - m)
//          case m => Pull.output(hd.take(n.toInt)) >> Pull.done
//        }
//      case None => Pull.done
//    }
//  }
//  in => go(in,n).stream
//}

def put(s: String): IO[Unit] = IO{println(s)}
def printRange: Stream[Pure, Int] = Stream.range(1, 10)

//evalMap means evaluate effects by function f for each element in original stream
val hello: Stream[IO, Unit] = printRange.evalMap[IO, Unit](x => put(x.toString))
hello.compile.drain.unsafeRunSync() // evaluate effects



