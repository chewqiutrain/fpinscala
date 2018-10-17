package fpinscala.errorhandling

import scala.{Option => _ }

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

/**Put functions for options inside a trait so we can call
  *  obj.fn(arg1) instead of fn(obj, arg1)
  */
sealed trait Option[+A] {
  // apply `f` if the option is not None
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(value) => Some(f(value))
      case _ => None
    }
  }

  // B >: A means B must be equal to or a supertype of A
  //  this is needed to tell the compiler that it is safe to declare
  //  Option[+A] as covariant in A
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(value) => value
      case _ => default
    }
  }

  // apply `f`, which may fail, to the Option[A] if it is not None
  // i.e. try to convert A to B using f, in the context of the Option.
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map[Option[B]](f).getOrElse(None)
  }

  // don't evaluate ob unless needed
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  // convert Some to None if the value doesn't satisfy `f`
  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    // if mean(xs) == None, flatMap will fail immediately
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // any function `f` can be transformed (via lift) to operate within the context
  //  of an option
  def lift[A, B](f: A => B): Option[A] => Option[B] = {
    _.map(f)
  }

  // non-idiomatic approach. map2 should not be `Option-aware`. i.e. it should
  //  not be unpacking the option. Use flatmap and/or map instead
  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a match {
      case None => None
      case Some(maybeA) => b match {
        case None => None
        case Some(maybeB) => Some(f(maybeA, maybeB))
      }
    }
  }

  //Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    // here, outermost flatMap A => Option[C]
    a.flatMap(maybeA =>
      // Inner must be Option[B] => Option[C], so we use map()
      b.map(maybeB =>
        f(maybeA, maybeB)
      )
    )
  }

  //Exercise 4.4
  // my implementation
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case head :: tail => {
        //head: Option[A], tail: List[Option[A]]
        head match {
          case None => None
          case Some(headVal) => Some(headVal :: sequence_1(tail).getOrElse(List.empty))
        }
      }
    }
  }

  // answer 1
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case head :: tail => head.flatMap(headVal => sequence(tail).map(listA => headVal :: listA))
    }
  }

  // answer 2
  def sequence_fold[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(List.empty))((someA: Option[A], acc: Option[List[A]]) => map2(someA, acc)(_ :: _))
  }


  //Exercise 4.5
  /*
    Sometimes, we want to map over a list, a function that can fail (i.e. return None if any element in the list returns None)
    We want to only traverse the list once.
   */
  // using sequence and list.map; inefficient because we traverse the list twice
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val x: List[Option[B]] = a.map(f)
    val y: Option[List[B]] = sequence[B](x)
    y
  }

  //try traversing only once
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case head :: tail => map2[B, List[B], List[B]](f(head), traverse(tail)(f))((x, y) => x :: y)
    }
  }

  def traverse_fold[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    //foldRight[B](z: B)(op: (A, B) => B): B
    a.foldRight[Option[List[B]]](Some(Nil))( (someA, acc) =>  map2(f(someA), acc)(_ :: _))
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse[Option[A], A](a)(x => x)
  }

  /// map via for comprehension
  def map2_for[A, B, C](a: Option[A], b: Option[B])(f : (A, B) => C): Option[C] = {
    // compiler desugars this into flatmap and map , the last binding is converted to map
    for {
      aa <- a //unpack A from Option[A]
      bb <- b
    } yield f(aa, bb) // result is still Option[f]
  }
}