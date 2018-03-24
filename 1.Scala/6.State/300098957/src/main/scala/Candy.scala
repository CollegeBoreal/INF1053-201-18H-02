
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

import State._

object Candy {

  def updateRule = (i: Input) => (s: Machine) => (i, s) match {
          case (_, Machine(_, 0, _))        => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _))  => s
          case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
          case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
        }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequenceViaFoldRight(inputs map (modify[Machine] _ compose updateRule))
      s <- get
    } yield (s.coins, s.candies)

  def main(args: Array[String]): Unit = {

    // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    val (a,state1) = simulateMachine(List(Coin)).run(Machine(locked = true, 1, 0))
    assert(!state1.locked)
    assert(a==(1,1)) // $1, 1 candy left

    // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    val (b,state2) = simulateMachine(List(Turn)).run(Machine(locked = false, 1, 1))
    assert(state2.locked)
    assert(state2.candies==0)
    assert(b==(1,0)) // $1, No candies left

    val (x,state3) = simulateMachine(List(Coin,Turn)).run(Machine(locked = true, 1, 1))
    assert(state3.locked)
    assert(state3.candies==0)
    assert(x==(2,0)) // $2, No candies left

  }

}