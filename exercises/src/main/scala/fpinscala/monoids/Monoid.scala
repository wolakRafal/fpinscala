package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }


  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1+a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1*a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2

    override def zero: (A) => A = identity
  }


  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen) {
      a => m.op(a, m.zero) == a
    } &&
    forAll(gen) {
      a => m.op(m.zero, a) == a
    }


  //  EXERCISE 5: Write a monoid instance for String that inserts spaces between words unless there already is one,
  // and trims spaces off the ends of the result. For example:
  def trimMonoid(s: String): Monoid[String] = new Monoid[String] {

    override def op(a1: String, a2: String): String = a1.trim + " " + a2.trim

    override def zero: String = ""
  }

  /** EXERCISE 6: Implement concatenate, a function that folds a list with a monoid: */
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a=> b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length < 10) {
      foldMap(as.toList, m)(f)
    } else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }


  // Creative Monoid for tuple: (the list, flag indicating if list is sorted)
  def orderedListMonoid[A]: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
  import Math.{min, max}
    override def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
      (a1,a2) match {
        case (x, None) => x
        case (None, x) => x
        case (Some((x1, y1, p)), Some((x2, y2, q))) => Some(min(x1, x2), max(y1, y2), p && q)
      }

    override def zero: Option[(Int, Int, Boolean)] = None
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    foldMapV(ints, orderedListMonoid)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }


  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    sys.error("todo")

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    sys.error("todo") 

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        val middleWord = if ((r1+l2).trim.isEmpty) 0 else 1 // check if we have word in the middle
        Part(l1, w1 + w2 + middleWord, r2)
      case (Part(l1, w1, r1), Stub(words)) =>
        Part(l1, w1, r1 + words)
      case (Stub(words), Part(l2, w2, r2)) =>
        Part(words + l2, w2, r2)
      case (Stub(words1), Stub(words2)) =>
        Stub(words1 + words2)
    }

    override def zero: WC = Stub("")
  }

  def count(s: String): Int = ???

  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(x: (A, B), y: (A, B)): (A, B) = (a.op(x._1, y._1), b.op(x._2,y._2))

      override def zero: (A, B) = (a.zero, b.zero)
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    override def op(f: (A) => B, g: (A) => B): (A) => B = a => B.op(f(a), g(a))

    override def zero: (A) => B = a => B.zero
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {

    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = a.map {
      case (k,v) => (k, V.op(v, b.getOrElse(k, V.zero)))
    }

    override def zero: Map[K, V] = Map()
  }


  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(endoMonoid[B])(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  def toList[A](fa: F[A]): List[A] = {
    foldRight(fa)(List.empty[A])(_ :: _)
  }

}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(f(a), b))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b,a) => mb.op(b,f(a)))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l,r) => mb.op(foldMap(l)(f)(mb),foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) : B = as match {
    case Leaf(a) => f(z,a)
    case Branch(l,r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }


  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l,r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a) => f(z,a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =as match {
    case None => z
    case Some(a) => f(a,z)
  }
}

