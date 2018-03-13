sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) =>  this
    case _ => None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m,2))))

  def main(args: Array[String]): Unit = {

    assert(mean(Seq(2.0,2.0,2.0))==Some(2.0))
    assert(Some(2.0).get==2.0)
    assert(mean(Seq())==None)
    val x = mean(Seq()) match { case Some(x) => x; case None => 0}; assert(x == 0)
    val y = mean(Seq(2.0)) match {case Some(x) => x case None => 0}; assert(y == 2.0)

    assert(Some(2.0).map((x:Double) => x + 1)==Some(3.0))
    assert(Some(2).getOrElse(1)==2); assert(None.getOrElse(1)==1)
    assert(Some(2.0).flatMap((x:Double) => None)==None)
    assert(Some(3).flatMap((x:Int) => Some(x * 2))==Some(6))
    assert(Some(2).orElse(Some(1))==Some(2)); assert(None.orElse(Some(1))==Some(1))
    assert(Some(5).filter(_ == 5)==Some(5))
    assert(variance(Seq(4.9,7.1,9.8,3.7,5.1))==Some(4.577600000000001))

  }
}
