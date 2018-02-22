

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
      case Branch(l,r) =>
    }

  def main(args: Array[String]): Unit = {

    //exe 3.25

    val t = Leaf(5);println(size(t))
    val t1 = Branch(Leaf('a'),Branch(Leaf('c'),Leaf('b')));println(size(t1))
    val t2 = Branch(Leaf(3),Branch(Leaf(0),Leaf(5)));println(max(t2))

  }
}