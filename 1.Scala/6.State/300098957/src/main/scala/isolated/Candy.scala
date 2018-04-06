package isolated

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

import scalaz._, std.list._, State._

object Candy {

  def updateRule(i: Input): Machine => Machine = (s: Machine) => (i, s) match {
    case (_, Machine(_, 0, _))        => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _))  => s
    case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
  }

  type MachineState[A] = State[Machine,A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- Applicative[MachineState].sequence(inputs.map(modify[Machine] _ compose updateRule))
      s <- get
    } yield (s.coins, s.candies)

  def main(args: Array[String]): Unit = {

    // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    val (state1,a) = simulateMachine(List(Coin)).run(Machine(locked = true, 1, 0))
    assert(!state1.locked)
    assert(a==(1,1)) // $1, 1 candy left

    // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    val (state2,b) = simulateMachine(List(Turn)).run(Machine(locked = false, 1, 1))
    assert(state2.locked)
    assert(state2.candies==0)
    assert(b==(1,0)) // $1, No candies left

    // the Input Machine has 1 coin and 1 candy, and 1 candy is successfully bought
    val (state3,x) = simulateMachine( Coin :: Turn :: Nil).run(Machine(locked = true, 1, 1))
    assert(state3.locked)
    assert(state3.candies==0)
    assert(x==(2,0)) // $2, No candies left

    // the Input Machine has 10 coins and 5 candies, and a total of 4 candies are successfully bought
    val (state4,y) = simulateMachine(List(Coin,Turn,Coin,Turn,Coin,Turn,Coin,Turn)).run(Machine(locked = true, 5, 10))
    assert(state4.locked)
    assert(state4.coins==14)
    assert(state4.candies==1)
    assert(y==(14,1)) // $14, 1 candy left

    // the Input Machine has 1 coin and 1 candy, and 1 candy is successfully bought
    // Discard the state
    val z = simulateMachine( Coin :: Turn :: Nil).eval(Machine(locked = true, 1, 1))
    assert(z==(2,0)) // $2, No candies left

    // the Input Machine has 1 coin and 1 candy, and 1 candy is successfully bought
    // Discard the state
    val state5 = simulateMachine( Coin :: Turn :: Nil).exec(Machine(locked = true, 1, 1))
    assert(state5.locked)
    assert(state5.candies==0)

  }

}
