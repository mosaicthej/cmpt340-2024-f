package q2
/*
 * Define a LazyList to generate an infinite list of prime numbers.  
 * Use higher-order functions whenever possible.
 * */

class Q2 {
  // Implementation of unfold
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): 
  LazyList[A] = f(z) match {
    case Some((h, s)) => h #:: unfold(s)(f)
    case None => LazyList.empty
  }
  // (a) Using unfold
  def iterateUsingUnfold[A](f: A => A)(x: A): LazyList[A] = {
    unfold(x)(x => Some((x, f(x))))
  }
  // (c) Generate an infinite list of prime numbers
  def primes: LazyList[Int] = {
    def sqrtF(n: Int): Int = {
      math.sqrt(n).floor.toInt
    }
    
    def isPrime(n: Int): Boolean = {
      if (n <= 1) false
      else if (n == 2) true
      else !(2 until sqrtF(n)+1).exists(x => n % x == 0)
    }
    /* after 2 and 3, primes are just every 2 numbers */      
    val primesAfter3 = iterateUsingUnfold[Int](_ + 2)(3).filter(isPrime)
    2 #:: primesAfter3
  }

  val primesWithUnfold = primes

  def isCoPrimeWith(ps: LazyList[Int], n:Int) : Boolean = {
      def go(pps:LazyList[Int]): Boolean = {
        if (pps.isEmpty) true
        else { 
          pps match {
          case h #:: t => 
            if (n % h == 0) false
            else if (h*h > n) true
            else go(t)
         }
        }
      }
    if (n<2) false
    else go(ps)
  }




  def iterateWithoutUnfold[A](f: A => A)(x: A): LazyList[A] = {
    x #:: iterateWithoutUnfold(f)(f(x))
  }

  val naturalNums = iterateWithoutUnfold[Int](_ + 1)(2)

  val primesWithoutUnfold : LazyList[Int] = 
    2 #:: naturalNums.filter(isCoPrimeWith(primesWithoutUnfold, _))



  

}
