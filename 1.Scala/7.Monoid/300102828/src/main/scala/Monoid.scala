trait Monoid[A]{
  def  op(a1: A, a2: A): A
  def zero: A
}

object Monoid{
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1+ a2
    val zero = 0
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1&& a2
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
    def op(f: A => A, g: A => A) = f compose g
    val zero = (a: A) => a
  }
    def main(args: Array[String]): Unit = {

      assert(stringMonoid.op("Mamoudou", "Sow") == "MamoudouSow")
      assert(listMonoid.op(List(1.0, 2.0), List(2.0, 4.0)) == List(1.0, 2.0, 2.0, 4.0))
      assert(intAddition.op(3, 4) == 7)
      assert(booleanAnd.op(3 % 2 == 0, 2 + 1 == 3) != booleanAnd.zero)
      assert(bitOr.op(1, 2) == 3)
      assert(bitAnd.op(1, 2) == bitAnd.zero)
      assert(optionMonoid[String].op(None, Some("Boreal")) == Some("Boreal"))
      assert(endoMonoid[Int].op((x: Int) => x - 12,(y: Int) => 2*y +3 )(4)== -1)
    }
  }

