package q3

/*
Problem 3 [20 Points]
A triple (x, y, z) of positive integers is pythagorean 
if x2 + y2 = z2. 

Using the functions studied in class, 
define a function pyth which returns the list of 
all pythagorean triples whose components are at most a given limit.  

For example, function call pyth(10) should return 
[(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)].  

One way to do this is to construct a list of all triples 
(use unfold to create a list of integers, 
  and then a for-comprehension to create a list of all triples), 
and then select the pythagorean ones.
*/

/* thanks for the hints */
class Q3 {

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


  def pyth(limit: Int): LazyList[(Int, Int, Int)] = {
    val range = 1 to limit
    val triples = for {
      x <- range.to(LazyList)
      y <- range.to(LazyList)
      z <- range.to(LazyList)
    } yield (x, y, z)
    triples.filter { case (x, y, z) => x*x + y*y == z*z }
  }
  
  def pythUnfold(limit: Int): LazyList[(Int, Int, Int)] = {
    lazy val range = iterateUsingUnfold[Int](_ + 1)(1)
    val triples = for {
      x <- range.takeWhile(_ <= limit)
      y <- range.takeWhile(_ <= limit)
      z <- range.takeWhile(_ <= limit)
    } yield (x, y, z)
  
    triples.filter  { case (x, y, z) => x*x + y*y == z*z }
  }

}

