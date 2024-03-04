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
case class Errors[+E](get: Seq[E]) extends Partial[E, Nothing] {
  def isSuccess: Boolean = false
  def isErrors: Boolean = true
}
case class Success[+A](get: A) extends Partial[Nothing, A] {
  def isSuccess: Boolean = true
  def isErrors: Boolean = false
}
