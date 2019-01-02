package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed: Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRng = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRng)
    }
  }

  //Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (r, rng2) = rng.nextInt
    if (r == Int.MinValue) {
      (r - 1, rng2)
    } else {
      (math.abs(r), rng2)
    }
  }

  //Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (r, rng2) = nonNegativeInt(rng)
    if (r == 0) {
      val y = ((r + 1)/Int.MaxValue).toDouble
      (y, rng2)
    } else {
      ((r/Int.MaxValue).toDouble, rng2)
    }
  }

  //Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  //Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (List.empty[Int], rng)
    } else {
      val (i, rngNext) = rng.nextInt
      val (nextLists, rngLast) = ints(count - 1)(rngNext)
      (i :: nextLists, rngLast)
    }
  }

  //So far, all our functions are of the following type. These represent state transitions, since they transform RNG
  // states from 1 to the next. They can be combined using combinators which are higher order functions. Think of this as
  // a program that depends on RNG, uses it to generate an A and also transitions the RNG to be used later
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  // we want to write combinators that allow us to combine Rand actions while avoiding explicitly passing along the RNG state
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i/(Int.MaxValue.toDouble + 1))

  //Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  //Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List.empty[A])){
        (f, acc) => map2(f, acc)((a, b) => b :: a)
    }

  //Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = // Rand[B] = RNG => (B, RNG)
    rng => {
      val (a, r1) = f(rng)
      val x: Rand[B] = g(a)
      val x2: (B, RNG) = g(a)(r1)
      x2
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    rng => {
      val (i, r1) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        (mod, r1)
      else nonNegativeLessThan(n)(r1)
    }

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    val (i, r1) = nonNegativeInt(r1)
    // f = RNG => (Int, RNG) = Rand[Int] ===> A = Int
    val inBounds: Int => Boolean = i => (i >= 0) && (i < n)
    flatMap(nonNegativeInt)(intI => {
      if (inBounds(intI)) unit(i)
      else nonNegativeLessThan2(n)
    })
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s){a =>
    val b = f(a)
    unit(b)
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){a =>
      flatMap(rb){b =>
        unit(f(a,b))
      }
    }


}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = ???
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ???
  def flatMap[B](f: A => State[S, B]): State[S, B] = ???
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candied: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}