package fpinscala.DataStructures

//Binary tree data structure
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  //Exercise 3.25
  // Counts the number of nodes (leaves and branches) in a tree
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  //Exercise 3.26
  //Returns maximum element in a Tree[Int]
  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(x) => x
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  //Exercise 3.27
  //returns maximum path length from root to any leaf
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }
  }

  //Exercise 3.28
  //modify each element in a tree given a function
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  //Exercise 3.29
  //Generalize size, maximum, depth, map by writing `fold` that abstracts over their similarities
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(x) => f(x)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold[A, Int](t)(tree1 => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int = fold[Int, Int](t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold[A, Int](t)(x => 0)((depthLeft, depthRight) => 1 + (depthLeft max depthRight))

  //NB: Type annotation for a => Leaf(f(a)): Tree[B] is required because without it, Scala infers the type as Leaf[B]
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(a => Leaf(f(a)): Tree[B])((leftResult, rightResult) => Branch(leftResult, rightResult))

}
