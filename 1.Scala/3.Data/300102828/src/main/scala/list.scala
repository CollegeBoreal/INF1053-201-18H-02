
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail :List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints  match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds  match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }


  def main(args: Array[String]): Unit = {
    val ex1: List[Int] = Nil;println(List.sum(ex1))
    val ex2 : List[Int] = Cons(1,Nil);println(List.sum(ex2))
    val ex3 : List[Int] = Cons(4,Cons(11,Nil));println(List.sum(ex3))
    val ex4 : List[Int] = Cons(4,Cons(11,Cons(20,Nil)));println(List.sum(ex4))
  }
}