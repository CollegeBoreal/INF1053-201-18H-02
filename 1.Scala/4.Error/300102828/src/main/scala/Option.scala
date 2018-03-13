import com.sun.xml.internal.ws.api.streaming.XMLStreamReaderFactory.Default

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]= this match {
    case None =>None
    case Some(a) => Some(f(a))
  }
  def getOrElse[B>: A](default: => B): B = this match {
    case None =>default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >:A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}
case class Some[+A](get:A) extends  Option[A]
case object None extends Option[Nothing]



object Option {



  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some (xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case e: Exception => None}

  def insuranceRateQuote( age: Int, numberOfSpeedingTickets: Int): Double = (age + numberOfSpeedingTickets) / 5

  def map2[A,B,C](a: Option[A],b: Option[B])(f:(A,B) => C): Option[C] = a flatMap(aa => b map(bb => f(aa,bb)))

  def map2_[A,B,C](a: Option[A],b: Option[B])(f:(A,B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)


  def parseInsuranceRateQuote( age: String, numberOfSpeedingTickets: String): Option[Double]= {
    val optAge: Option[Int] = Try {age.toInt}
    val optTicket: Option[Int] = Try  { numberOfSpeedingTickets.toInt}
   map2_(optAge, optTicket)(insuranceRateQuote)
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] =  as match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap (xx => sequence(xs) map (xxs => xx::xxs))
  }

  def sequence_[A](as: List[Option[A]]): Option[List[A]] =  as match {
    case Nil => Some(Nil)
    case x :: xs => for (xx <- x; xxs <- sequence(xs)) yield xx :: xxs
  }



  def main(args: Array[String]): Unit = {

    assert(mean(Seq(2.0,2.0,2.0)) == Some(2.0))
    assert(Some(2.0).get==2.0)

    assert(mean(Seq()) == None)

    val x = mean(Seq()) match {case Some(x) => x case None => 0}; assert(x == 0)
    val y = mean(Seq(2.0)) match {case Some(x) => x case None => 0};assert(y == 2.0)

    assert(Some(2.0).map((x:Double) => x + 1) == Some(3.0))
    assert(Some(2).getOrElse(1)==2); assert(None.getOrElse(1)==1)
    assert(Some(2.0).flatMap((x: Double) => None)== None)
    assert(Some(3).flatMap((x: Int) => Some(x * 2))== Some(6))
    assert(Some(2).orElse(Some(1))== Some(2)); assert(None.orElse(Some(1))== Some(1))
    assert(Some(5).filter(_==5)== Some(5))
    assert(variance(Seq(4.9,7.1,9.8,3.7,5.1))== Some(4.577600000000001))
    assert(Try("elf".toInt)==None)
    assert(parseInsuranceRateQuote("25","10")== Some(7.0)); assert(parseInsuranceRateQuote("elf","9")== None)
    assert(sequence(List(Some(1),Some(2),Some(3)))== Some(List(1,2,3)))
    assert(sequence_(List(Some(1),Some(2),Some(3)))== Some(List(1,2,3)))

  }

}
