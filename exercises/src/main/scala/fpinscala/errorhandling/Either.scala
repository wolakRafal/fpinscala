package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(a) => Right(f(a))
   case Left(b) => Left(b)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => f(a)
   case Left(b) => Left(b)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => Right(a)
   case Left(l) => b
 }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    a <- this
    b1 <- b
  } yield f(a, b1)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case x::xs => f(x).flatMap(a => traverse(xs)(f).map(bs => a::bs))
  }

  def traverse1[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case Right(x) :: xs => sequence(xs).map(x :: _)
    case Left(e) :: xs => Left(e)
  }

  def sequence_1[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}