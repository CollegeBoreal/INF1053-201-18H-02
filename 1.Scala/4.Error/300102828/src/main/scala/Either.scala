sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
  Either[EE, C] = for { a <- this; b1 <- b }
    yield f(a,b1)
}



case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[ Nothing,A]

case class Person (name: Name, age: Age)
sealed class Name(val value:  String)
sealed class  Age(val value: Int)

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("Mean of Empty List!")
    else Right(xs.sum / xs.length)

  def saveDiv(x: Int, y: Int): Either[Exception, Int]=
    try Right(x/y)
    catch {
      case e: Exception => Left(e)
    }

  def mkName(name: String): Either[String,Name]=
    if (name == " "||name== null) Left("Name is Empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String,Age]=
    if (age < 0 ) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person]=
    mkName(name).map2(mkAge(age))(Person(_,_))


  def main(args: Array[String]): Unit = {

    val m = mean(IndexedSeq(1.0,3.0,7.0,5.0))
    assert(m.map((x: Double) => x - 1.0)==Right(3.0))
    assert(m.flatMap((x: Double) => Right(x +1.0)) == Right(4.0))
    assert(saveDiv(5,0).orElse(Left(-5))==Left(-5))
    assert(m.map2(Right(4.0))(_+_) == Right(8.0))
    assert(m.map2(Left("x"))(_+_).orElse(Left("x")) == Left("x"))
    assert(mkPerson(name = "Mamoudou",age = 25)
      .orElse(Left("Batman"))
        .map(_.name.value == "Mamoudmou")
    == Right(true))

    assert(mkPerson(name = "Brice",age = -1) == Left("Age is out of range."))

  }
}
