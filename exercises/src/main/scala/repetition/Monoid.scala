package repetition

trait Monoid[A] {

  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {

    override def op(f1: (A) => A, f2: (A) => A): (A) => A = f1 andThen f2

    override def zero: (A) => A = identity[A]
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  //  Implemetation foldLeft using foldMap
  def foldLeftFM[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

}