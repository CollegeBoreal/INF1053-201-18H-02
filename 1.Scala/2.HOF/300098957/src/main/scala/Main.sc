val x = 5

def f(x: Int) = { x*x }

f(4)

def f(x: Any) = println(x)


type R = Double

def f(x: R) = x * x

val f1 = f(3.2)

def g(x: => R) = {  x  }

g(f(6.8))

(x: R) => x * x  // Fonction Anonyme

import scala.language.postfixOps

(1 to 5).map(2*)

(1 to 3).reduceLeft(_ + _)

(1 to 5) filter {_%2 == 0} map {_*2}

(1 to 5) filter {0 == 2%_} map {2*}

// Composition

def o(g:R=>R, h:R=>R) = (x:R) => g(h(x))

val z = o({2*}, {1-})

// Currying

val zscore = (mean:R, sd:R) => (x:R) => (x-mean)/sd

zscore(34.1,15.9)(5)

def _zscore(mean:R, sd:R) = (x:R) => (x-mean)/sd

_zscore(34.1,15.9)(5)

def __zscore(mean:R, sd:R)(x:R) = (x-mean)/sd

__zscore(34.1,15.9)(5)

(x: Int) => 1 + x

