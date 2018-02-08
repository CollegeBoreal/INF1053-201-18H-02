object Partial {

  def partial[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a,b)

  // Exercice 2.3
  // sachant que A => ( B => C) peut etre ecrit A => B => C
  def curry[A,B,C](f: (A, B) => C): A => ( B => C) = (a: A) => (b: B) => f(a,b)

  // Exercice 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A , b: B) => f(a)(b)

  // Exercice 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))


  def main(args: Array[String]): Unit = {
    val fSansA = partial(2, (a: Int,b: Double) => b + 1)
    val fAvecA = partial(2, (a: Int,b: Double) => a + b)
    println(fSansA(3.0))
    println(fAvecA(3.0))
    println(curry((a:Int,b: Double) => a - b)(3)(4.0))
    println(compose((b: Double) => b + 4,(a: Double) => a * 2)(4))
  }

}
