package fpinscala.laziness

import fpinscala.laziness.Stream._

import scala.collection.mutable.ListBuffer

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument
  // by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def toList_usingFold: List[A] = foldRight(List.empty[A])((a, z) => a :: z)

  // Optimal solution
  def toList: List[A] = {
    val buf = new ListBuffer[A]
    def rec(s: Stream[A]): List[A] = {
      s match {
        case Cons(h, t) =>
          buf += h()
          rec(t())
        case Empty => buf.toList
      }
    }
    rec(this)
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream.
    // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None // opr
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h,_) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  def take_unfold(n: Int): Stream[A] = unfold((n, this)) {
    case (m, Cons(h, t)) if m > 0 => Some(h(), (m - 1, t()))
    case (0, _) => None
    case (_, Empty) => None
  }

  def drop(n: Int): Stream[A] = {
    def goRec(m: Int, s: => Stream[A]): Stream[A] = s match {
      case Cons(_, t) if m > 1 => goRec(m - 1, t())
      case Cons(_, t) if m == 1 => t()
      case _ => Empty
    }
    goRec(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile_unfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  // using fold right
  def takeWhile_fold(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a,s) => if(p(a)) cons(a,s) else empty)


  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case Empty => true
  }

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(h, t), Cons(h2, t2)) if h() == h2() => t().startsWith(t2())
    case (_, Empty) => true
    case _ => false
  }

  def starts_2[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(!_._2.isEmpty).forAll {
    case (h1, h2) => h1 == h2
  }


  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h,t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)

  def map_unfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  // zip using unfold ,
  def zip[B](s2: Stream[B]): Stream[(A, B)] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this, s2) {
    case (Empty, Empty) => None
    case (Cons(h1, t1), Empty) => Some((Some(h1()), Option.empty[B]), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((Option.empty[A], Some(h2())), (Empty, t2()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s => Some((s, s.drop(1)))
    case Empty => None // ???
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // natural numbers
  def nat(start: Int): Stream[Int] = Stream.cons(start, nat(start + 1))

  def constant[A](a : A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs = {
    def fibRec(f1: Int, f2: Int): Stream[Int] = {
      cons(f1 + f2, fibRec(f2, f1 + f2))
    }
    cons(0, cons(1, fibRec(0, 1)))
  }

  def fibs_unfold = unfold((0, 1)) { case (f0, f1) => Some(f0 + f1, (f1, f0 + f1)) }

  def from_unfold(n: Int): Stream[Int] = unfold(n)(m => Some(m, m + 1))

  def constant_unfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def ones_unfold: Stream[Int] = unfold(1)(_ => Some(1, 1))


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, newState)) => cons[A](h, unfold(newState)(f))
    case None => empty[A]
  }


}