import scala.annotation.tailrec
package q3
class Q3 {
/*
Problem 3 [20 Points]. A higher-order function unfold can be defined as follows to encapsulate a pattern of recursion for producing a list:
*/
def unfold[A, B] (p: A => Boolean, h: A => B, t: A => A)
                 (x: A) : List[B] = if (p(x)) Nil
                    else h(x) :: unfold(p, h, t) (t(x))
/*
That is, the function call unfold(p, h, t, x) produces the empty list if the predicate p is true of x,
and otherwise produces a non-empty list by applying the function h to x to give the head, 
and the function t to x to generate another argument 
that is recursively processed in the same way to produce the tail of the list.
Define the following functions using unfold:
    - p: predicate to check if x is terminating
    - h: generate head
    - t: what's to do with x to next step (tail)
*/
/*
(a) [10 Points]. 
A function, int2bin, 
to convert integers to binary numbers.
[Hint: Repeatedly dividing the integer by 2 can give values of bits for the binary number]
*/
def int2bin(n: Int): List[Int] = {
  def p(x: Int): Boolean = x == 0 /* terminate when x is 0 */
  def h(x: Int): Int = x % 2 /* the head is the remainder (lowest bit) */
  def t(x: Int): Int = x / 2 /* the tail is the quotient (shift right) */
  unfold(p, h, t)(n).reverse /* reverse the list to get correct bit order */
}

/*
(b) [10 Points]. 
A function, repHalve, 
where repHalve takes list l, 
and returns a list of lists containing the elements of l, 
so that the first list contains half of l’s elements, 
the next contains half of the remaining elements, 
and so on.  

For example, 
repHalve (List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)) should return:
List(List(1, 2, 3, 4, 5, 6, 7, 8), List(9, 10, 11, 12), List(13, 14), List(15))
*/
    def takeN[A](l: List[A], n: Int): List[A] = l match {
        case Nil => Nil
        case car :: cdr => if (n == 0) Nil else car :: takeN(cdr, n - 1)
    }

    def dropN[A](l: List[A], n: Int): List[A] = l match {
        case Nil => Nil
        case car :: cdr => if (n == 0) cdr else dropN(cdr, n - 1)
    }

    def repHalve[A](l: List[A]): List[List[A]] = {
        println("repHalve with " + l)
        def p(x: List[A]): Boolean = (x.length == 0)
        def h(x: List[A]): List[A] = {
            /* return first half of the list */
            println("h with " + x)
            val n = {
                if (x.length % 2 == 0) x.length / 2
                else ( (x.length+1) / 2)
            }
            takeN(x, n)
        }
        def t(x: List[A]): List[A] = {
            println("t with " + x)
            /* return second half of the list */
            val n = x.length / 2
            dropN(x, n)
        }
        unfold(p, h, t)(l)
    }
}