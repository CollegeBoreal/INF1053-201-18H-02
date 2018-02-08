object Polymorphic {
   def findFirst(ss: Array[String], key: String): Int = {
      def loop(n: Int): Int =
        if (n >= ss.length) - 1
        else if (ss (n) == key) n
        else loop(n+1)

    loop(0)
  }
  def findFirst(is : Array[Int], key:Int) : Int = {
    def loop(n: Int): Int =
      if (n>= is.length) -1
      else if (is(n) == key) n
      else loop(n+1)
    loop(0)

  }
  def findFirst[A](as:Array[A],p: A => Boolean):Int = {
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)

  }
  def main (arg: Array[String]): Unit = {
    println("ID : " + findFirst(Array("moi","toi","lui","ell"), "lui"))
    println("ID :" + findFirst(Array(1,4,6,2), 6))
    println("ID :" + findFirst(Array(5.2, 3.1, 2.0, 5.1),(x: Double) => x == 2.0))
  }


}
