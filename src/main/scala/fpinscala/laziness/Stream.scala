package fpinscala.laziness

import scala.{Stream => _}
import scala.List

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h()) // explicit forcing of h thunk using h()
  }


  // is this stack safe?
  @annotation.tailrec // compiles; it is safe?
  private def helperToList[A](es: Stream[A])(acc: List[A]): List[A] = {
    es match {
      case Empty => acc
      case Cons(h, t) => helperToList(t())(acc ++ List(h()) )
    }
  }

  def toList: List[A] = helperToList(this)(List.empty[A])

  // Exercise 5.2
  private def helperToTake[A](orig: Stream[A])(acc: Stream[A])(n: Int): Stream[A] = {
//    println(n.toString)
    orig match {
      case Cons(h, t) if n > 0 => helperToTake[A](t())(Stream.cons(h(), acc))(n - 1)
      case Cons(h, t) if n <= 0 => acc
      case Empty => acc
    }
  }

  private def reverseHelper[A](curr: Stream[A])(acc: Stream[A]): Stream[A] = {
    curr match {
      case Cons(h, t) => reverseHelper[A](t())(Stream.cons(h(), acc))
      case Empty => acc
    }
  }

  def reverse(): Stream[A] = reverseHelper(this)(Empty)

  def take(n: Int): Stream[A] = helperToTake[A](this)(Empty)(n).reverse()


  private def dropHelper[A](curr: Stream[A])(n: Int): Stream[A] = {
    curr match {
      case Cons(h, t) if n > 0 => dropHelper(t())(n - 1)
      case Cons(h, t) if n <= 0 => curr
      case Empty => curr
    }
  }

  // Exercise 5.3
  def drop(n: Int): Stream[A] = dropHelper(this)(n)

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) =>
        if (p(h())) Stream.cons(h(), t().takeWhile(p)) else Empty
      case Empty => Empty
    }
  }


  // `||` is non-strict. i.e, if p(h()) evaluate to true, then exists terminates the traversal
  // early and returns true. since the tail is a lazy val, it is never evaluated.
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // notice that the 2nd argument to f is CBN, so it may not be evaluated
  // if f() does not evaluate its 2nd argument, then the recursion never occurs
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // implement exists using foldRight
  // lazy evaluation allows us to use foldRight to implement exist so that the traversal
  // is terminated early. the strict version of foldRight does not allow us to do this
  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  //Exercise 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  //Exercise 5.5
  // not sure if this is using lazy correctly. OK!
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    def fac(a: A, b: => Stream[A]): Stream[A] = if (p(a)) Stream.cons(a, b) else Empty
    val acc: Stream[A] = Empty
    foldRight(acc)(fac)
  }

  //Exercise 5.6
  def headOptionViaFoldRight: Option[A] = {
    foldRight[Option[A]](None)((a, b) => Some(a))
  }


  //def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
  //Exercise 5.7 OK!
  def map[B](f: A => B): Stream[B] = {
    foldRight[Stream[B]](Empty)((a, accsb) => Stream.cons(f(a), accsb)) // can do this though
  }

//  OK!
  def filter(f: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Empty)((a, acc) => if (f(a)) Stream.cons(a, acc) else acc)
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => Stream.cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight[Stream[B]](Empty)((a, acc) =>
      f(a) match {
        case Cons(h, t) => Stream.cons(h(), acc)
        case Empty => acc
      })
  }

  //Exercise 5.13
  //def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
  // let S = A and A = B

//  Stream[A] => Option[(B, Stream[A])]
  private def mapHelperF[A1, B](f: A1 => B)(in: Stream[A1]): Option[(B, Stream[A1])] =
    in match {
      case Cons(h, t) => Some(f(h()), t())
      case _=> None
    }


  //correct
  def mapViaUnfold[B](f: A => B): Stream[B] = {
//    val x: Stream[A] => Option[(B, Stream[A])] = mapHelperF(f)
    Stream.unfold[B, Stream[A]](this)(mapHelperF(f))
  }

//  use (Stream[A], Int) to track state
  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold[A, (Stream[A], Int)]((this, n)){
    case (Cons(h, t), i) if i > 0 => Some(h(), (t(), i - 1))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold[A, Stream[A]](this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold[C, (Stream[A], Stream[B])]((this, s2)){
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case (_, _) => None
    }

  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((this, s2)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Empty, Empty) => None
    }

  //Exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean = zipAllViaUnfold(s).takeWhileViaUnfold(_._2.nonEmpty).forAll{
    case (h1, h2) => h1 == h2
  }

  //Exercise 5.15
  //Stream(1,2,3) returns Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())
  // must use unfold
  def tails: Stream[Stream[A]] = Stream.unfold[Stream[A], Stream[A]](this){
    case Cons(h, t) => Some((Cons(h, t), t()))
    case Empty => None
  }.append(Stream.empty)

  def hasSubsequence[A](s: Stream[A]): Boolean = tails.exists(_.startsWith(s))


  //Exercise 5.16
  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right.
  It can be implemented using `foldRight` though.
  The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results,
  which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is
  needed to compute the result. Here, we simply extract the accumulated list once finished.
  */
  //def foldRight[B](z: => B)(f: (A, => B) => B): B
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2: B = f(a, p1._1)
      val x = Stream.cons(b2, p1._2)
      (b2, x)
    })._2
}


case object Empty extends Stream[Nothing]

/*
() => A is a function that accepts 0 arguments and returns an A === Function0[A] AKA a THUNK

Due to technical limitations (?) We must explicitly force these thunks rather than by-name params
 */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream{
  /*
  'smart' constructor -> function for constructing a data type that ensures some additional invariant  than the
  real constructors used for pattern matching
  usually lowercase the corresponding data constructor
   */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def flatten[A](in: Stream[Stream[A]]): Stream[A] = {
    in.foldRight[Stream[A]](empty[A])((sa, acc) => sa match {
      case Cons(h, t) => cons(h(), acc)
      case Empty => acc
    })
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  //Exercise 5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  //Exercise 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  //Exercise 5.10
  private def fibHelper(x1: Int, x2: Int): Stream[Int] = {
    val next = x1 + x2
    Stream.cons(x2, fibHelper(x2, next))
  }

  def fibs(): Stream[Int] = Stream.cons(0, fibHelper(0,1))

  //Exercise 5.11
  /* Corecursive function (i.e. produces instead of consumes data)
  * Need not terminate as long as they remain `productive` (as long as f terminates with a valid result)
  * Corecursion == guarded recursion
  * Productivity == cotermination
  * */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val maybeResult: Option[(A, S)] = f(z)
    maybeResult match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Stream.empty[A]
    }
  }

  //Exercise 5.12

//  private def fibUnfoldHelper: Int => Option[(Int, Int)] = (state: Int) => if (state == 0) Some(0,1) else Some()
  def fibHelperCurried(n1: Int)(n2: Int): Option[(Int, Int)] = Some((n1, n1 + n2))

//  def fibsViaUnfold(): Stream[Int] = unfold[Int, Int](1)(fibHelperCurried())

  def fromViaUnfold(n: Int): Stream[Int] = unfold[Int, Int](n)(x => Some((x, x + 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold[A, A](a)(x => Some(x, x))

  val onesViaUnfold: Stream[Int] = constantViaUnfold[Int](1)



}