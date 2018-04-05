package isolated

import scalaz._, Scalaz._

case class Container(i: Int)

object Container {

  def compute(s: String): State[Container, Int] = State {
    case Container(i) => (Container(i + 1), s.toInt + i)
  }

  type ContainerState[A] = State[Container, A]

  def main(args: Array[String]): Unit = {
    assert(List("1", "2", "3").traverse[ContainerState, Int](x => compute(x)).run(Container(0))==(Container(3),List(1,3,5)))
  }
}
