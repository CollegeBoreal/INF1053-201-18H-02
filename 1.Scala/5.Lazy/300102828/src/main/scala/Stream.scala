sealed trait Stream[+A]{
  def heaOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }
  def toListRecursive: List[A] = this match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A ,t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] ={
    lazy val head = hd
    lazy val  tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def main(args: Array[String]): Unit = {

    assert(empty.heaOption==None)
    assert(Stream(1,2,3,4).heaOption==Some(1))
    assert(Stream(1,2,3,4).toListRecursive==List(1,2,3,4))
    assert(Stream(1,2,3,4).toList==List(1,2,3,4))
  }



}

