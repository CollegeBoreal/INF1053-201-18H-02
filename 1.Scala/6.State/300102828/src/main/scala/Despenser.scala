
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
import State._

object Dispenser {
    def rule = (i: Input) => (s: Machine) => (i, s) match {
      case (Turn,Machine(true, _,_ )) => s
      case (Coin, Machine(false, _, _)) => s
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)

      }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose rule))
    s <- get
  } yield (s.coins, s.candies)

    def main(args: Array[String]): Unit = {
      val s1 = Machine(locked = true, 10,10)
      assert(simulateMachine(List(Turn)).run(s1) == ((10,10),s1))
      val s2 = Machine(locked = true, 100,0)
      assert(simulateMachine(List(Turn)).run(s2) == ((0,100),s2))
      assert(simulateMachine(List(Coin)).run(Machine(locked = true, 10,5))._1._1== 6)
      assert(simulateMachine(List(Turn)).run(Machine(locked = false, 5,10))._1._2== 4)
      val action = List(Coin,Turn,Coin,Turn,Coin,Turn,Coin,Turn)
      assert(simulateMachine(action).run(Machine(locked = true, 5, 10))._1==(14,1))



    }


}