package laziness

import scala.{Stream => _ }

import fpinscala.laziness.Stream
import org.scalacheck.Properties
import org.scalacheck.Prop._


class StreamSpec extends Properties("Laziness - Streams") {
  property("Stream.toList") = {
    val x = List(1, 2, 3, 4)
    val xstream: Stream[Int] = Stream.apply(x: _*)
    val back2x: List[Int] = xstream.toList

    back2x == x
  }

  property("Stream.reverse") = {
    val x = List(1, 2, 3, 4)
    val xstream: Stream[Int] = Stream.apply(x: _*)
    val reversed: List[Int] = xstream.reverse().toList

    reversed == List(4,3,2,1)
  }

  property("Stream.take(n)") = {
    val x = List(1,2,3,4)
    val streamForTake: Stream[Int] = Stream.apply(x: _*)
    val n = 2
    val take2 = streamForTake.take(n)
    val take2List = take2.toList

    take2List == List(1,2)
  }

  property("Stream.drop(n)") = {
    val x = List(1,2,3,4)
    val streamI: Stream[Int] = Stream.apply(x: _*)
    val n = 3
    val dropStream = streamI.drop(n)
    val dropStreamToList = dropStream.toList

    dropStreamToList == List(4)
  }

  property("Stream.takeWhile") = {
    val x = List(9,2,4,22,-1,15,10,3,11,6)
    val streamI: Stream[Int] = Stream.apply(x: _*)
    val f: Int => Boolean = (i: Int) => i > 5
    val res = streamI.takeWhile(f).toList

    val x2 = List(9,2,4,22,-1,15,10,3,11,6)
    val streamI2: Stream[Int] = Stream.apply(x2: _*)
    val f2: Int => Boolean = (i: Int) => i > 0
    val res2 = streamI2.takeWhile(f2).toList

    res2 == List(9,2,4,22) &&
      res == List(9)
  }

  property("Stream.takeWhileViaFoldRight") = {
    val x = List(9,2,4,22,-1,15,10,3,11,6)
    val streamI: Stream[Int] = Stream.apply(x: _*)
    val f: Int => Boolean = (i: Int) => i > 5
    val res = streamI.takeWhileViaFoldRight(f).toList

    val x2 = List(9,2,4,22,-1,15,10,3,11,6)
    val streamI2: Stream[Int] = Stream.apply(x2: _*)
    val f2: Int => Boolean = (i: Int) => i > 0
    val res2 = streamI2.takeWhileViaFoldRight(f2).toList

    res2 == List(9,2,4,22) &&
      res == List(9)
  }

  property("Stream.map") = {
    val x: List[Int] = List(1,2,3,4,5,6,7,8,9,10)
    val xmapdouble: List[Int] = x.map(_ * 2)

    val streamX: Stream[Int] = Stream.apply(x: _*)
    val streamXmapDouble: Stream[Int] = streamX.map(_ * 2)
    val listStreamXmapDouble: List[Int] = streamXmapDouble.toList
    xmapdouble == listStreamXmapDouble
  }

  property("Stream.filter") = {
    val x: List[Int] = List(1,2,3,4,5,6,7,8,9,10)

    val xMapFilter: List[Int] = x.filter(_ % 2 == 0)

    val streamX: Stream[Int] = Stream.apply(x: _*)

    val streamFilter: Stream[Int] = streamX.filter(_ % 2 == 0)

    val streamFilterList: List[Int] = streamFilter.toList

    streamFilterList == xMapFilter
  }

  property("Stream.flatMap") = {
    val x: List[Int] = List(1,2,3,4,5,6,7,8,9,10)
    val xFlatMapDouble: List[Int] = x.flatMap(xi => List(xi * 2))

    val streamX: Stream[Int] = Stream.apply(x: _*)

    val streamXflatMapDouble: Stream[Int] = streamX.flatMap(xi => Stream(xi * 2))

    val streamXflatMapList: List[Int] = streamXflatMapDouble.toList

    streamXflatMapList == xFlatMapDouble
  }

  property("Stream.constant") = {
    val infStream1: Stream[Int] = Stream.constant(1)
    val sixOnes = infStream1.take(6).toList

    sixOnes == List(1,1,1,1,1,1)
  }

  property("Stream.from") = {
    val infStream1: Stream[Int] = Stream.from(76)
    val n = 6

    val res1 = infStream1.take(n).toList

    val expected1 = List(76, 77, 78, 79, 80, 81)

    res1 == expected1
  }

  property("Stream.append") = {
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)
    val s1 = Stream(l1: _*)
    val s2 = Stream(l2: _*)
    val s3 = s1.append(s2)
    val l3 = l1 ::: l2
    s3.toList == l3
  }

  property("Stream.fibs - Infinite Fibonacci sequence") = {
    val expectedFibs = List(0,1,1,2,3,5,8,13,21,34)
    val mainFibs = Stream.fibs()

    val n = 3
    val expectedFibsN = expectedFibs.take(n)
    val testFibs = mainFibs.take(n).toList

    val n2 = 7
    val testFibs2 = mainFibs.take(n2).toList
    val expectedFibsN2 = expectedFibs.take(n2)

    testFibs == expectedFibsN &&
      testFibs2 == expectedFibsN2
  }

  property("Stream.onesViaUnfold") = {
    val infStream1: Stream[Int] = Stream.onesViaUnfold
    val sixOnes = infStream1.take(6).toList

    sixOnes == List(1,1,1,1,1,1)
  }

  property("Stream.constantViaUnfold") = {
    val infStream1: Stream[Int] = Stream.constant(42)
    val res1 = infStream1.take(6).toList

    res1 == List(42,42,42,42,42,42)
  }

  property("Stream.fromViaUnfold") = {
    val infStream1: Stream[Int] = Stream.fromViaUnfold(76)
    val n = 6

    val res1 = infStream1.take(n).toList

    val expected1 = List(76, 77, 78, 79, 80, 81)
    println(res1)
    res1 == expected1
  }

  property("Stream.mapViaUnfold") = {
    val x: List[Int] = List(1,2,3,4,5,6,7,8,9,10)
    val xmapdouble: List[Int] = x.map(_ * 2)

    val streamX: Stream[Int] = Stream.apply(x: _*)
    val streamXmapDouble: Stream[Int] = streamX.mapViaUnfold(_ * 2)
    val listStreamXmapDouble: List[Int] = streamXmapDouble.toList
    xmapdouble == listStreamXmapDouble
  }

  property("Stream.takeViaUnfold(n)") = {
    val x = List(1,2,3,4)
    val streamForTake: Stream[Int] = Stream.apply(x: _*)
    val n = 2
    val take2 = streamForTake.takeViaUnfold(n)
    val take2List = take2.toList

    val n2 = 0
    val streamForTake2: Stream[Int] = Stream.apply(x: _*)
    val take0 = streamForTake2.takeViaUnfold(n2)
    val take0List = take0.toList

    take2List == List(1,2) &&
    take0List == List.empty[Int]
  }

}

