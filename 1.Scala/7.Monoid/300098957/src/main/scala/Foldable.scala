import scala.language.postfixOps

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
}

object Foldable {

  object listFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)( (b,a) => mb.op(b,f(a)))
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object treeFoldable extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a,z)
      case Branch(l,r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z,a)
      case Branch(l,r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(a) => f(a)
      case Branch(l,r) => mb.op(foldMap(l)(f)(mb),foldMap(r)(f)(mb))
    }
  }

  import Monoid._

  def main(args: Array[String]): Unit = {
    assert(listFoldable.foldRight(List(1,2,3))(intAddition.zero)((x: Int, y: Int) => intAddition.op(x,y))==6)
    assert(listFoldable.foldLeft(List(1,2,3))(intAddition.zero)(intAddition.op)==6)
    assert(listFoldable.foldMap(List(1,2))(_ - 1)(intAddition)==1)
    assert(listFoldable.foldMap(List(1,2))(1+)(intAddition)==5)
    assert(treeFoldable.foldRight(Leaf(1))(intAddition.zero)((x: Int, y: Int) => intAddition.op(x,y))==1)
    assert(treeFoldable.foldLeft(Branch(Leaf(1),Leaf(2)))(intAddition.zero)((x: Int, y: Int) => intAddition.op(x,y))==3)
    assert(treeFoldable.foldMap(Branch(Leaf(1),Branch(Leaf(2),Leaf(1))))((x: Int) => intAddition.op(x,1))(intAddition)==7)
  }
}

