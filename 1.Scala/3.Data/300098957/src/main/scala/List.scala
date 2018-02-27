import scala.annotation.tailrec
import scala.util.control.TailCalls.TailRec

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

  def foldRight3[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, _) if x == 0.0 => z // Not working
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns,0)(_+_)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_*_)

  def product3(ns: List[Double]) = foldRight3(ns, 1.0)(_*_)

  def length[A](as: List[A]): Int = List.foldRight(as,0)((_, acc) => acc + 1)

  def sum4(as: List[Int]): Int = foldLeft(as,0)(_+_)

  def prod4(as: List[Double]): Double = foldLeft(as,1.0)(_*_)

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B):B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft( xs, f(z,x))(f)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((acc,h) => Cons(h,acc))
  }

  def append[A](as: List[A])(elem: A): List[A] = {
    def loop(xs: List[A], acc: List[A]): List[A] =
      xs match {
        case Nil => Nil
        case Cons(x, Nil) => Cons(elem,Cons(x,acc))
        case Cons(x, xs) => loop(xs, Cons(x, acc)  )
      }

    reverse(loop(as, Nil))
  }

  def append1[A](as: List[A])(ls: List[A]): List[A] =
    foldRight(as, ls)((h,t) => Cons(h,t))

  def append2[A](as: List[A])(ls: List[A]): List[A] =
    foldLeft(as, ls)((t,h) => Cons(h,t))

  def append3[A](as: List[A], ls: List[A]): List[A] =
    foldRight(as, ls)((h,t) => Cons(h,t))

  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append3)

  def add(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((x, xs) => Cons( x + 1, xs))

  def doubleToString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((x, xs) => Cons(x.toString, xs))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((x, xs) => Cons(f(x),xs))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((x, xs) => if (f(x)) Cons(x,xs) else xs)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) Cons(x,Nil) else Nil)

  def zipWith(as: List[Int], ls: List[Int]): List[Int] = (as,ls) match {
    case (Nil,_) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipWith(t1,t2))
  }

  def zipWith[A, B, C](as: List[A])(ls: List[B])(f: (A, B) => C): List[C] = (as,ls) match {
    case (Nil,_) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1)(t2)(f))
  }

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

    // Exercise   3.7
    val ex10: List[Double] = List(1.0,2.0,0.0,4.0,5.0)
    println(product3(ex10))

    // Exercise   3.8
    println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    println(List(1,2,3) == foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

    // Exercise 3.9
    println(List.length(ex8))


    // Exercise 3.10
    println(foldLeft(List(1,2,3), 0)(_+_))
    println(foldLeft(List(2.0,3.0,4.0), 1.0)(_*_))

    // Exercise 3.11
    println(sum4(ex8))
    println(prod4(ex9))

    // Exercise 3.12
    val ex11: List[Int] = List(1,2,3)
    println(reverse(ex11))

    // Exercise 3.14
    println(append(ex8)(6))
    println(append1(ex8)(ex11))
    println(append2(ex8)(ex11))

    // Exercise 3.15
    val ex12: List[List[Int]] = List(List(1,2,3),List(1,2,3))
    println(concat(ex12))

    // Exercise 3.16
    println(add(ex8))

    // Exercise 3.17
    println(doubleToString(ex9))

    // Exercise 3.18
    println(map(ex8)(_ - 1))

    // Exercise 3.19
    println(filter(ex8)(_ % 2 != 0))

    // Exercise 3.20
    println(flatMap(ex11)(i => List(i,i)))

    // Exercise 3.21
    println(filter2(ex8)(_ % 2 == 0))

    // Exercise 3.22
    println(zipWith(List(1,2,3),List(4,5,6)))

    // Exercise 3.23
    println(zipWith(List(1,2,3))(List(4,5,6))(_+_))

  }

}



