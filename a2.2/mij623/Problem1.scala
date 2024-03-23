package q1
object Problem1 {
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

  // (b) Without using unfold
  def iterateWithoutUnfold[A](f: A => A)(x: A): LazyList[A] = {
    x #:: iterateWithoutUnfold(f)(f(x))
  }
}
