package isolated

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

import scalaz._, std.list._, syntax.traverse._


object Candy {

  val m: State[Machine, (Int,Int)] = State[Machine, (Int,Int)]{
    m => (Machine(false, m.candies + 1, m.coins + 1 ), (m.candies + 1, m.coins + 1))
  }

  def updateRule(i: Input): State[Machine, (Int,Int)] = State[Machine, (Int,Int)]{
    (m: Machine) => (i,m) match {
      case (_, Machine(_, 0, _))        => (m, (m.candies, m.coins))
      case (Coin, Machine(false, _, _)) => (m, (m.candies, m.coins))
      case (Turn, Machine(true, _, _))  => (m, (m.candies, m.coins))
      case (Coin, Machine(true, candy, coin)) => (Machine(locked = false, candy, coin + 1 ), (candy, coin + 1))
      case (Turn, Machine(false, candy, coin)) => (Machine(locked = true, candy - 1, coin),(candy - 1, coin))
    }
  }

  type MachineState[A] = State[Machine,A]

  def main(args: Array[String]): Unit = {

    assert(m.eval(Machine(locked = true, 0, 0))==(1,1))
    assert(m.exec(Machine(locked = true, 0, 0))==Machine(locked = false, 1, 1))
    assert(updateRule(Turn).exec(Machine(locked = false, 1, 0))==Machine(locked = true, 0, 0))
    assert(updateRule(Coin).eval(Machine(locked = true, 1, 0))==(1, 1))

    assert(List(Coin, Turn).traverse[MachineState,(Int,Int)](updateRule).eval(Machine(locked = true, 1, 0))==List((1,1), (0,1)))

  }

}
