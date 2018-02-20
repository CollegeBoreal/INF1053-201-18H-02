sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds  match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons( _, xs) => xs
    }

  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => Cons(a,Nil)
    case Cons( _, xs) => Cons(a, xs)
  }

  def drop[A](as: List[A], n: Int): List[A] =
    if (n <= 0) as
    else as match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => as
    }

  def init[A](as: List[A]): List[A] = {
    def reverse(xs: List[A], acc: List[A]): List[A] =
      xs match {
        case Nil => acc
        case Cons(x, xs) => reverse(xs, Cons(x, acc)  )
      }

    def loop(xs: List[A], acc: List[A]): List[A] =
      xs match {
        case Nil => Nil
        case Cons(x, Nil) => acc
        case Cons(x, xs) => loop(xs, Cons(x, acc)  )
      }

    reverse(loop(as, Nil), Nil)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns,0)(_+_)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_*_)

  def main(args: Array[String]): Unit = {

    // Somme
    val ex1: List[Int] = Nil; println(List.sum(ex1))
    val ex2: List[Int] = Cons(1, Nil); println(List.sum(ex2))
    val ex3: List[Int] = Cons(4, Cons(11,Nil)); println(List.sum(ex3))
    val ex4: List[Int] = Cons(4, Cons(11, Cons( 20, Nil))); println(List.sum(ex4))

    // Produit
    val ex5: List[Double] = Nil; println(List.product(ex5))
    val ex6: List[Double] = Cons(1.0, Cons(0.0, Nil)); println(List.product(ex5))
    val ex7: List[Double] = Cons(4, Cons(11, Cons( 20, Nil))); println(List.product(ex7))

    // Constructeur apply
    val ex8: List[Int] = List(1,2,3,4,5); println(List.sum(ex8))

    val ex81: List[Int] = List(2,4,3,1,5)

    val ex9: List[Double] = List(1.0,2.0,3.0,4.0,5.0)

    val x = List(1,2,3,4,5) match {
      case Cons(x,Cons(2, Cons(4 , _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3 ,Cons(4, _)))) => x + y
      case Cons(h,t) => h + sum(t)
      case _ => 101
    }
    println(x)

    // Exercise 3.2
    println(List.tail(ex8))

    // Exercise 3.3
    println(List.setHead(6,ex8))

    // Exercise 3.4
    println(drop(ex8, 2))

    // Exercise 3.5
    println(dropWhile(ex8, (x: Int) => x < 4))
    println(dropWhile(ex81, (x: Int) => x < 4))

    // Exercise 3.6
    println(init(ex8))

    println(sum2(ex8))

    println(product2(ex9))


  }

}



