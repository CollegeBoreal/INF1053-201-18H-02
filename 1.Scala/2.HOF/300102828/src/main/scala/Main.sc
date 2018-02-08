type R = Double

var x = 5
val y = 5
def f(x: Int) = {x*x

}
def f(x: Any) =
  print(x)
//def f(x: R)vs.
def g(x: => R)

(1 to 5).map { x => val
y=x*2; println(y); y}

(1 to 5) filter {_%2 == 0} map {_*2}


import scala.language.postfixOps
(1 to 5).map(2 *)

val zscore = (mean:R, sd:R) => (x:R) => (x-mean)/sd
zscore(1,3)(5)
def _zscore(mean:R, sd:R) = (x:R) => (x-mean)/sd
_zscore(1,3)(5)

def __zscore(mean:R, sd:R)(x:R) = (x-mean)/sd
__zscore(1,3)(5)