package q2

import scala.util.Success
/* Problem 2 [25 Points]. 
  Recall that unlike Options 
  where multiple errors are simply subsumed by a None, 
  when we deal with multiple errors in Eithers, 
    all but one are lost.  
  
  Implement a datatype to return back multiple errors.   
  
  Pay close attention to situations which do and 
    do not require recording of multiple errors.  

  You may use the following definition 
  for the purpose of defining your data type. */

/* Now, implement the following functions for this datatype.
accumulating errors when meaningful to do so. */

/*
  Per my design, map3, map2, and sequence are
  the only ones that errors can be accumulated.
*/

sealed class Partial[+E, +A] {
    /* a) [3 Points] map 
    which applies the given function to the value in the Success case. */
    def map[nA](f: A=>nA): Partial[E, nA] = /* Error or f(x) */
      this match {
        /*
            case Errors(e_seq) => append a new error to the sequence
            case Success(s) => try Success(f(s)) but catch the exception
        */
        case Errors(e_seq) => Errors(e_seq) // todo: accumulate errors? (what type)?
        case Success(s) => Success(f(s))
    }

    /* b) [3 Points] flatMap
    which applies the given function to the value in the Success case 
    and returns the result. */
    def flatMap[AA >: A, C](f: A => Partial[E, A]): Partial[E, A] = this match {
        case Errors(e_seq) => Errors(e_seq)
        case Success(s) => f(s)
    }

case class Errors[+E](get: Seq[E]) extends Partial[E, Nothing] {
  def isSuccess: Boolean = false
  def isErrors: Boolean = true
}
case class Success[+A](get: A) extends Partial[Nothing, A] {
  def isSuccess: Boolean = true
  def isErrors: Boolean = false
}
