/** ***************************************************************************
  * Copyright (c) 2015
  * ADVA Optical Networking
  *
  * All rights reserved. Any unauthorized disclosure or publication of the
  * confidential and proprietary information to any other party will constitute
  * an infringement of copyright laws.
  *
  * $Id$
  * Author  : Rafal Wolak, RWolak@advaoptical.com
  * Created : 31 March 2015
  * Purpose :
  *
  * $Rev$
  * $URL$
  *
  * Notes:
  *
  * ****************************************************************************
  */

package fpinscala.testing

import fpinscala.state.{State, RNG}

/** First Exercises of chapter 8 using type alias for Gen[A] */
object SimpleGen {

  object Gen {

    def unit[A](a: => A): Gen[A] = Gen()

  }

  case class Gen[+A](sample: State[RNG,A], exhaustive: Stream[A])



  def choose(start: Int, stopExclusive: Int): Gen[Int] =

}

