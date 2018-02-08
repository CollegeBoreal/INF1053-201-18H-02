object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }


def fib(n: Int): Int={
  def loop (n:Int,prev:Int, cur: Int): Int =
    if (n<= 0) prev
    else loop(n-1,cur, prev+cur)
  loop(n,0,1)

}

  private def formatResult(name: String, x: Int, f: Int => Int) = {
    val msg = "the %s of %d is %d"
    msg.format(name, x, f(x))
  }

  private def formatFactorial(x: Int) = {
    val msg = "factorial of %d is %d"
    msg.format(x, factorial(x))
  }



def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("formatFactorial", 3, factorial))
    println(formatResult("fib", 5, fib))
  }
}

