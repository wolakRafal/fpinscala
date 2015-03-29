package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) - (i +1 ) else i, r)
  }

  // wrong implementation, returns double from 0 to 1 inclusive <0,1>
  def double_wrong(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, r)
  }

  // returns double in range <0,1)
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val id = intDouble(rng)
    (id._1.swap, id._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def intsRec(count: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = count match {
      case 0 => (acc, r)
      case n =>
        val (i, rngNext) = r.nextInt
        intsRec(n - 1, rngNext, i :: acc)
    }
    intsRec(count, rng, List())
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //  EX. 5 Use map to generate an Int between 0 and n, inclusive:
  def positiveMax(n: Int): Rand[Int] =  {
    def f(a: Int): Int = a % (n + 1)

    map(int)(f)
  }

//  EXERCISE 6: Use map to reimplement RNG.double in a more elegant way
  def _double : Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // EXERCISE 7: Unfortunately, map is not powerful enough to implement
  //  intDouble and doubleInt from before. What we need is a new combinator
  //  map2, that can combine two RNG actions into one using a binary rather than unary
  //    function. Write its implementation and then use it to reimplement the intDouble
  //  and doubleInt functions.
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def _intDouble: Rand[(Int, Double)] = map2(nonNegativeInt, double) { case (i, d) => (i, d) }

  def _doubleInt: Rand[(Double, Int)] = map2(double, nonNegativeInt) { case (d, i) => (d, i) }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = (rng: RNG) => fs.foldRight((List.empty[A], rng)) {
    case (ra: Rand[A], (acc: List[A], rNext: RNG)) =>
      val (a, r2) = ra(rNext)
      (a :: acc, r2)
  }

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // More understandable
  def flatMap_understandable[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    (rng: RNG) => {
      val (a, rng2) = f(rng)
      val (b, rngb) = g(a)(rng2)
      (b, rngb)
    }

  // short, harder to grasp
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  // reimplement positive Int
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

//  EXERCISE 10: Reimplement map and map2 in terms of flatMap.
def _map[A, B](r: Rand[A])(f: A => B): Rand[B] = flatMap[A,B](r)(f.andThen(unit))
// other solution, the same
def __map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) {
    a => map(rb) { b =>
      f(a, b)
    }
  }

// using for comprehension
//  def __map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = for {
//    a <- ra
//    b <- rb
//  } yield f(a,b)



}
// Conclusion
// map does not care that it is  dealing with RNG state actions and we can give it a more general signature:

//    def map[S, A, B](s: S => (A, S))(f: A => B): S => (B, S)

// come up with a more general type than Rand, for handling any
// type of state:
//        type State[S, +A] = S => (A, S)
//
// or even better, we can write it as its own class, wrapping the underlying function like this:

case class State[S,+A](run: S => (A, S)) {

  //  EXERCISE 11: Generalize the functions unit, map, map2, flatMap, and sequence
  // using flatMap
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
//
  def map2_[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- this
    b <- sb
  } yield f(a, b) //State.unit(c)

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B](s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  // we could just make Rand a type alias for State:
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

//  ???
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(unit[S, List[A]](List())) {
    case (f, acc) => f.map2(acc)(_ :: _)
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = unit(s)
//  def set[S](s: S): State[S, Unit] = State(s=> ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    val inputStates = inputs.map(i => modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candies, coins)) =>
        Machine(locked = false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) =>
        Machine(locked = true, candies - 1, coins)
    }))

    for {
      _ <- sequence(inputStates)
      s <- get
    } yield (s.candies, s.coins)
  }

}
