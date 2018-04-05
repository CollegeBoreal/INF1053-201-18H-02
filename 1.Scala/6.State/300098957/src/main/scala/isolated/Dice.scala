package isolated

// https://stackoverflow.com/questions/7734756/scalaz-state-monad-examples

import scalaz._, std.list._, syntax.traverse._

object Dice {

  // Create a state computation incrementing the state and returning the "str" value
  val s = State[Int, String](i => (i + 1, "str"))

  import java.util.Random

  def dice() = State[Random, Int](r => (r, r.nextInt(6) + 1))

  def TwoDice() = for {
    r1 <- dice()
    r2 <- dice()
  } yield (r1, r2)

  type StateRandom[A] = State[Random,A]

  val list = List.fill(10)(TwoDice())
  // List[scalaz.IndexedStateT[scalaz.Id.Id,Random,Random,(Int, Int)]]
  val list2 = list.sequence[StateRandom, (Int,Int)]
  // list2: StateRandom[List[(Int, Int)]] = ...


  case class Machine(locked: Boolean, candies: Int, coins: Int)

  val m = State[Machine, (Int,Int)](m => (Machine(false, m.candies + 1, m.coins + 1 ), (m.candies + 1, m.coins + 1)))


  def main(args: Array[String]): Unit = {

    // start with state of 1, pass it to s
    assert(s.eval(1)=="str")
    // returns result value "str"

    // same but only retrieve the state
    assert(s.exec(1)==2)
    // 2

    // get both state and value
    assert(s(1)==(2, "str"))
    assert(s.run(1)==(2, "str")) // or s.run(1)
    // (2, "str")

    assert(TwoDice().eval(new Random(1L))==(4,5))

    // run this computation starting with state new Random(1L)
    val tenDoubleThrows2 = list2.eval(new Random(1L))
    assert(tenDoubleThrows2==List((4,5), (2,4), (3,5), (3,5), (5,5), (2,2), (2,4), (1,5), (3,1), (1,6)))
    // tenDoubleThrows2  : scalaz.Id.Id[List[(Int, Int)]] =
    //   List((4,5), (2,4), (3,5), (3,5), (5,5), (2,2), (2,4), (1,5), (3,1), (1,6))

    assert(m.eval(new Machine(locked = true, 0, 0))==(1,1))
    assert(m.exec(new Machine(locked = true, 0, 0))==Machine(locked = false, 1, 1))


  }

}





