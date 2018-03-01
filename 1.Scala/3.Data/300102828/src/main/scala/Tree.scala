

sealed trait Tree[+A]
case class Leaf[A](value: A) extends  Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t:Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l,r) => size(l) + size(r)

    }
  def max(t: Tree[Int]): Int =
    t match {
      case Leaf(m) => m
      case Branch(l,r) => max(l) max max(r)

    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l,r) => 1 + (depth(l) max depth(r))
    }

  //exe 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  //exe 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold[A](t: Tree[A])(p: (A,A)=>A): A =
    fold(t)(a => a)(p)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

  def mapViaFold[A](t: Tree[A])(f: A => A): Tree[A] =
  fold(t)(a => Leaf(f(a)): Tree[A])(Branch(_,_))



  def main(args: Array[String]): Unit = {

    //exe 3.25

    val t = Leaf(5);println(size(t))
    val t1 = Branch(Leaf('a'),Branch(Leaf('c'),Leaf('b')));println(size(t1))
    val t2 = Branch(Leaf(3),Branch(Leaf(0),Leaf(5)));println(max(t2))
    val t3= Branch(Leaf(3),Branch(Leaf(0),Leaf(5)));println(depth(t3))
    val t4= Branch(Branch(Leaf(1),Leaf(2)),Leaf(3));println(map(t4)(_+ 1))
    assert(maximumViaFold(t4)(_ max _)==3)
    assert(depthViaFold(t4)==2)
    assert(mapViaFold(t4)( _ + 1)==Branch(Branch(Leaf(2),Leaf(3)),Leaf(4)))


  }
}