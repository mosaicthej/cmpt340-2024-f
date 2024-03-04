package q2

import scala.util.{Success=>_, Failure=>_, _}
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
        case _ => throw new Exception("This should not happen")
    }

    /* b) [3 Points] flatMap
    which applies the given function to the value in the Success case 
    and returns the result. */
    def flatMap[EE >: E, C](f: A => Partial[E, A]): Partial[E, A] = this match {
        case Errors(e_seq) => Errors(e_seq)
        case Success(s) => f(s)
        case _ => throw new Exception("This should not happen")
    }

    /* c) [3 Points] getOrElse
    which returns the value in the Success case,
    or the given default value if it is an Errors. */
    def getOrElse[AA >: A](default: => AA): AA = this match {
        case Errors(e_seq) => default
        case Success(s) => s
        case _ => throw new Exception("This should not happen")
    }

    /* d) [3 Points] orElse
    which returns the first Success case it finds,
    or, if there are no Success cases, an Errors. */
    def orElse[AA >: A, EE >: E](default: => Partial[EE, AA]): Partial[EE, AA] = this match {
        case Errors(e_seq) => default
        case Success(s) => Success(s)
        case _ => throw new Exception("This should not happen")
    }

    
    /* e) [3 Points] map3 (to lift a function of three arguments) 
    as opposed to map2 which two arguments get lifted. */
    def map3[EE >:E, B, C, D]
    (b: Partial[EE, B], c: Partial[EE, C])
    (f: (A, B, C) => D): Partial[E, D] = {
      this match {
        case Success(aa) => b match {
          case Success(bb) => c match {
            case Success(cc) => Success(f(aa, bb, cc))
            case Errors(e_seq_c) => Errors(e_seq_c)
            case _ => throw new Exception("This should not happen")
          }
          case Errors(e_seq_b) => c match {
            case Success(cc) => Errors(e_seq_b)
            case Errors(e_seq_c) => Errors(e_seq_b ++ e_seq_c)
            case _ => throw new Exception("This should not happen")
          }
          case _ => throw new Exception("This should not happen")
        }
        case Errors(e_seq) => b match {
          case Success(bb) => c match {
            case Success(cc) => Errors(e_seq)
            case Errors(e_seq_c) => Errors(e_seq ++ e_seq_c)
            case _ => throw new Exception("This should not happen")
          }
          case Errors(e_seq_b) => c match {
            case Success(cc) => Errors(e_seq ++ e_seq_b)
            case Errors(e_seq_c) => Errors(e_seq ++ e_seq_b ++ e_seq_c)
            case _ => throw new Exception("This should not happen")
          }
          case _ => throw new Exception("This should not happen")
        }
        case _ => throw new Exception("This should not happen")
      }
    }

    /*
    which combines two Partials using a binary function. */
    def map2[EE >: E, B, C](b: Partial[EE, B])(f: (A, B) => C): Partial[EE, C] = {
      this match {
        case Errors(e_seq) => b match {
          case Errors(e_seq_b) => Errors(e_seq ++ e_seq_b)
          case Success(bb) => Errors(e_seq)
          case _ => throw new Exception("This should not happen")
        }
        case Success(aa) => b match {
          case Errors(e_seq_b) => Errors(e_seq_b)
          case Success(bb) => Success(f(aa, bb))
          case _ => throw new Exception("This should not happen")
        }
        case _ => throw new Exception("This should not happen")
      }
    }

    /* f) [6 Points] sequence
    which combines a list of Partials into one Partial
      containing a list of all the Success values in the list.
    If the original list contains any Errors, the result of the function
      should be an Errors which all errors collected. */
    def containErrors[E, A](ps: List[Partial[E, A]]): Boolean = 
      ps match {
        case Nil => false
        case h::t => h match {
          case Errors(e_seq) => true
          case Success(s) => containErrors(t)
          case _ => throw new Exception("This should not happen")
        }
        case _ => throw new Exception("This should not happen")
      }

    def sequence[E, A](ps: List[Partial[E, A]]): Partial[E, List[A]] = 
      if (containErrors(ps)) {
        val errors = ps.foldLeft(Seq[E]())((acc, p) => p match {
          case Errors(e_seq) => acc ++ e_seq
          case Success(s) => acc
          case _ => throw new Exception("This should not happen")
        })
        Errors(errors)
      } else { // a list of all successes
        val successes = ps.map(p=> p match {
          case Success(s) => s
          case _ => throw new Exception("This should not happen")
        })
        Success(successes)
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

/*
Errors and Success are disjoint unions, a Partial can be 
either an Errors or a Success but not both.

Errors -> get: yields a sequence of errors
Success -> get: yields a value of type B (no errors at all)
*/
object Partial {
/* g) [2 Points] 
Implement the Try function to convert a possible exception into a Partial
*/
  def Try[A](a: => A): Partial[Exception, A] = 
    try Success(a)
    catch { case e: Exception => Errors(Seq(e)) }

}
