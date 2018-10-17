package fpinscala.DataStructures

/* `+` indicates A is covariant. i.e.
if X <: Y (X is a subtype of Y), then List[X] <: List[Y] */
sealed trait List[+A]
//possible forms of List. Empty, and Cons
//List data constructor representing the empty list
case object Nil extends List[Nothing]
//Another data constructor representing non-empty list, tail can be Nil or another Cons
case class Cons[+A](head: A, tail: List[A]) extends List[A]

//List companion object, contains functions for creating and working with Lists
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // variadic function syntax
  /*
  * `as` is bound as a Seq[A], which has .head and .tail
  * `_*` is a special type that allows us to pass a Seq to a variadic method
  * */
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  //Exercise 3.1; result = 3
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4,_)))) => x + y
    case _ => 101
  }

  //Exercise 3.2: O(1)
  def tail[A](as: List[A]): List[A] = as match {
    // taking the tail of an empty list should be a bug, not a Nil.
    case Cons(_, Nil) => sys.error("Tail of empty list")
    case Cons(_, tail) => tail
  }

  //Exercise 3.3: O(1)
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Cons(_, Nil) => sys.error("called setHead on empty list")
    case Cons(_, tail) => Cons(a, tail)
  }

  //Exercise 3.4: O(n). Remove first n elements from a list
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) tail(l)
    else l match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n-1)
    }
  }

  //Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l // covers the cases (1)if l is Nil, or (2) if !f(h)
    }
  }

  //curried version means we don't need to pass a type parameter for the argument of f.
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Cons(h,t) if f(h) => dropWhile2(t)(f)
      case _ => as
    }
  }

  //Runtime and memory dependent only on length of a1.
  //results in a1 ::: a2
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  //Exercise 3.6: O(n) because we are always copying the entire list up till the last element when calling recursively
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("Can't remove last element of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  //currying allows the type of f to be inferred.
  //allows further abstraction of sum and product
  // NB: foldRight needs to traverse all the way to the end of the list before collapsing it.
  //  frames are being pushed onto the call stack. not stack safe
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = foldRight(ns,0)((x,y) => x + y)
  //_*_ is concise notation for (x,y) => x * y

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  /* Exercise 3.7
  Can product2 immediately halt the recursion and return 0.0 if it encounters a 0.0?
  Ans: No. When we call f, we immediately evaluate the arguments to f, one of which is foldRight.
  which means we will traverse the list all the way to the end. Need non-strict evaluation to support early termination
  * */

  /* Exercise 3.8
  See what happens when you pass Nil and Cons into foldRight. i.e. foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
  --> Cons(1, Cons(2, Cons(3, Nil))) => Cons(1, foldRight(Cons(2,Cons(3,Nil)), Nil: List[Int])(Cons(_,_)
  --> Cons(1, Cons(2, foldRight(Cons(3,Nil), Nil: List[Int])(Cons(_,_)) )
  --> Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil: List[Int])(Cons(_,_)))
  --> Cons(1, Cons(2, Cons(3, Nil)))

  We get back the same list.
  it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with
  the given function, `f`.
   */

  //Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  //Exercise 3.10
  // foldRight is not stack safe, this is.
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }
  }

  //Exercise 3.11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length3[A](l: List[A]): Int = foldLeft(l, 0)((acc , _) => acc + 1)

  //Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

  //Exercise 3.13
  // fuking gg
  def foldLeftByRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = {
    //def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B
//    foldRight(as, (b: B) => b)((a,g) => g(f(b,a)))(z)
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    foldLeft(reverse(as), z)((b,a) => f(a,b))
  }

  //Exercise 3.14
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft[A, List[A]](a1, a2)((acc: List[A], h: A) => Cons(h, acc))

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight[A, List[A]](a1, a2)((h: A, acc: List[A]) => Cons(h, acc))

  //Exercise 3.15
  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil: List[A])(append)
    //`(acc: List[A], h: List[A]) => append(h, acc)` can be written as just `append`
  }

  //Exercise 3.16
  def intPlus1(as: List[Int]): List[Int] =
    foldRight[Int, List[Int]](as, Nil: List[Int])((h: Int, acc: List[Int]) => Cons(h+1, acc))

  //Exercise 3.17
  def doubleToString(as: List[Double]): List[String] =
  //def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B
    foldRight[Double, List[String]](as, Nil: List[String])((h: Double, acc: List[String]) => Cons(h.toString, acc))

  //Exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft[A, List[B]](as, Nil: List[B])((h: A, acc: List[B]) => Cons(f(h), acc))

  //Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
//    foldRightViaFoldLeft[A, List[A]](as, Nil: List[A])((h: A, acc: List[A]) => Cons(h, dropWhile(acc,f)))
  foldRightViaFoldLeft[A, List[A]](as, Nil: List[A])((h: A, acc: List[A]) => if (f(h)) Cons(h, acc) else acc)

  //Exercise 3.20
  def flatMap0[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight[A, List[B]](as, Nil: List[B])((h: A, acc: List[B]) => append(f(h), acc))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  //Exercise 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((h: A) => if (f(h)) List(h) else Nil)

  //Exercise 3.22
  def addPairWise(as1: List[Int], as2: List[Int]): List[Int] = {
    (as1, as2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
    }
  }

  def zipWith[A](as1: List[A], as2: List[A])(f: (A,A) => A): List[A] = {
    (as1, as2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
    }
  }

  //Exercise 3.24
  //my failed attempt
  def hasSubsequence0[A](sup: List[A], sub: List[A]): Boolean = {
    val originalSub = sub
    def helper(source: List[A], reducedSub: List[A]): Boolean = {
      (source, reducedSub) match {
        case (Nil, _) => ???
        case (Cons(h1,t1), Cons(h2, t2)) if h1 == h2 => helper(t1, t2)
      }
    }
    //placeholder cause i gave up
    true
  }

  //answer
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }

}

