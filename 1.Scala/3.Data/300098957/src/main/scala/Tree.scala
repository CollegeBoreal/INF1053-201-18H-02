import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //@tailrec
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => max(l) max max(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(n) => 0
    case Branch( l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(a => 1)(_+_)

  def main(args: Array[String]): Unit = {

    // Exercice 3.25
    val t = Leaf(5); assert(size(t)==1)
    assert(size(Branch(Leaf('a'),Branch(Leaf('c'),Leaf('b'))))==3)

   // Exercice 3.26
    assert(max(Branch(Branch(Leaf(3),Leaf(0)),Branch(Leaf(5),Leaf(8))))==8)
    assert(max(Branch(Leaf(3),Branch(Leaf(0),Leaf(5))))==5)

    // Exercice 3.27
    assert(depth(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3)))==2)

    // Exercice 3.28
    val t4 = Branch(Branch(Leaf(1),Leaf(2)),Leaf(3)); println(map(t4)(_ + 1))

    assert(sizeViaFold(Branch(Leaf('a'),Branch(Leaf('c'),Leaf('b'))))==3)

  }
}


