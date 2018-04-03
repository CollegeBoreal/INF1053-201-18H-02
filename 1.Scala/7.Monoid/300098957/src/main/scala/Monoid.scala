trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  val bitAnd: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 & a2
    val zero = 0
  }

  val bitOr: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 | a2
    val zero = 0
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def zero: A => A = (a: A) => a
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
  }

  def main(args: Array[String]): Unit = {

    assert(stringMonoid.op("Sow","Boreal")=="SowBoreal")
    assert(listMonoid.op(List(1.0,2.0),List(2.0,4.0))==List(1.0,2.0,2.0,4.0))
    assert(intAddition.op(3,4)==7)
    assert(booleanAnd.op(3 % 2 == 0, 2 + 1 == 3)!=booleanAnd.zero)
    assert(bitAnd.op(1,2)==bitAnd.zero)
    assert(bitOr.op(1,2)==3)
    assert(optionMonoid.op(Some(2),Some(3))==Some(2))
    assert(optionMonoid[String].op(None,Some("Boreal"))==Some("Boreal"))
    assert(endoMonoid[Int].op((x: Int) => x - 12,(y: Int) => 2 * y + 3)(4) == -1)

  }
}
