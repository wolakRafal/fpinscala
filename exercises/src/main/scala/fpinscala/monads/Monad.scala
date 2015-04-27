package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = {
    lma.foldRight(unit(List[A]()))((ma: M[A], mb: M[List[A]]) => map2(ma,mb)(_ :: _))
  }

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = {
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))
    // OR in terms of sequence -> sequence(la.map(f))
  }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  def factor[A,B](ma: M[A], mb: M[B]): M[(A,B)] = map2(ma,mb)((_, _))

  def cofactor[A,B](e: Either[M[A], M[B]]): M[Either[A, B]] = e match {
    case Left(ma) => map(ma)(Left(_))
    case Right(mb) => map(mb)(Right(_))
  }

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???
}


object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] =
      Par.flatMap(ma)(f)

    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

//  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma.flatMap(f)

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      ma.flatMap(f)

    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] =
      ma.flatMap(f)

    override def unit[A](a: => A): List[A] = List(a)
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S,x]})#lambda] {
    override def unit[A](a: => A): State[S, A] = State(s => (a,s))

    override def flatMap[A, B](st: State[S, A])(f: (A) => State[S, B]): State[S, B] =
    st flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)

    override def unit[A](a: => A): Id[A] = Id(a)
  }

}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(r => a)
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
       Reader(r => f(st.run(r)).run(r))
  }
}

