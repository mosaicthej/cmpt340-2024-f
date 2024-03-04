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

    /* c) [3 Points] getOrElse
    which returns the value in the Success case,
    or the given default value if it is an Errors. */
    def getOrElse[AA >: A](default: => AA): AA = this match {
        case Errors(e_seq) => default
        case Success(s) => s
    }

    /* d) [3 Points] orElse
    which returns the first Success case it finds,
    or, if there are no Success cases, an Errors. */
    def orElse[AA >: A, EE >: E](default: => Partial[EE, AA]): Partial[EE, AA] = this match {
        case Errors(e_seq) => default
        case Success(s) => Success(s)
    }

    
    /* e) [3 Points] map3 (to lift a function of three arguments) 
    as opposed to map2 which two arguments get lifted. */
    def map3[EE >:E, B, C, D]
    (b: Partial[E, B], c: Partial[E, C])
    (f: (A, B, C) => D): Partial[E, D] = {
      this match {
        case Success(aa) => b match {
          case Success(bb) => c match {
            case Success(cc) => Success(f(aa, bb, cc))
            case Errors(e_seq_c) => Errors(e_seq_c)
          }
          case Errors(e_seq_b) => c match {
            case Success(cc) => Errors(e_seq_b)
            case Errors(e_seq_c) => Errors(e_seq_b ++ e_seq_c)
          }
        }
        case Errors(e_seq) => b match {
          case Success(bb) => c match {
            case Success(cc) => Errors(e_seq)
            case Errors(e_seq_c) => Errors(e_seq ++ e_seq_c)
          }
          case Errors(e_seq_b) => c match {
            case Success(cc) => Errors(e_seq ++ e_seq_b)
            case Errors(e_seq_c) => Errors(e_seq ++ e_seq_b ++ e_seq_c)
          }
        }
      }
    }

case class Errors[+E](get: Seq[E]) extends Partial[E, Nothing] {
  def isSuccess: Boolean = false
  def isErrors: Boolean = true
}
case class Success[+A](get: A) extends Partial[Nothing, A] {
  def isSuccess: Boolean = true
  def isErrors: Boolean = false
}
