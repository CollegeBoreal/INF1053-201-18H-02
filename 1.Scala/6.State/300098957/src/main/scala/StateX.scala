
import StateX._

case class StateX[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): StateX[S, B] = flatMap(a => unit(f(a)))
  def map2[B,C](sb: StateX[S, B])(f: (A, B) => C): StateX[S, C] = for (a <- this; b <- sb) yield f(a,b)
  def flatMap[B](f: A => StateX[S, B]): StateX[S, B] = StateX(s => { val (a, s1) = run(s); f(a).run(s1) })
}

object StateX {

  def unit[S, A](a: A): StateX[S, A] = StateX(s => (a, s))

  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequenceViaRecursion[S, A](sas: List[StateX[S, A]]): StateX[S, List[A]] = {
    def go(s: S, actions: List[StateX[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    StateX((s: S) => go(s,sas,List()))
  }

  // We can also write the loop using a left fold. This is tail recursive like the
  // previous solution, but it reverses the list _before_ folding it instead of after.
  // You might think that this is slower than the `foldRight` solution since it
  // walks over the list twice, but it's actually faster! The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequenceViaFoldLeft[S,A](l: List[StateX[S, A]]): StateX[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

   // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S,A](sas: List[StateX[S, A]]): StateX[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  // let's pick one implementation
  def sequence[S, A](sas: List[StateX[S, A]]): StateX[S, List[A]] = sequenceViaFoldRight(sas)

  // desugarized
  def modifyT[S](f: S => S): StateX[S, Unit] = get.flatMap(s => set(f(s)) )

  def modify[S](f: S => S): StateX[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: StateX[S, S] = StateX(s => (s, s))

  def set[S](s: S): StateX[S, Unit] = StateX(_ => ((), s))

}
