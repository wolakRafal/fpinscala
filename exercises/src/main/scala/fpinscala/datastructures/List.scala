package fpinscala.datastructures

import scala.collection.mutable.ListBuffer

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Another data constructor, representing nonempty lists.
// Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("head of empty list")
    case Cons(h, t) => l
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case ll@Cons(x, xs) => n match {
      case 0 => ll
      case m => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case ll@Cons(x, xs) => if (f(x)) dropWhile(xs, f) else ll
  }

  def init[A](l: List[A]): List[A] = {
    val buf = new scala.collection.mutable.ListBuffer[A]
    def go(ls: List[A]): List[A] = ls match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => buf += h; go(t)

    }
    go(l)
  }

  def length_1[A](l: List[A]): Int = {
    def count(ls: List[A], acc: Int): Int = {
      ls match {
        case Nil => acc
        case Cons(x, xs) => count(xs, acc + 1)
      }
    }
    count(l, 0)
  }

  def length[A](l: List[A]): Int = foldRight(l,0)((_,z)=>z+1)


    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    def mapRec(ls: List[A], acc: List[B]): List[B] = ls match {
      case Nil => acc
      case Cons(h, t) => mapRec(t, Cons(f(h), acc))
    }
    mapRec(l, Nil)
  }
}