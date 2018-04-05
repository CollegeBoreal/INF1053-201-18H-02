sealed trait Action
case object Piastre extends Action
case object Tourner extends Action
case object Annuler extends Action

case class Bonbonniere(bloque: Boolean, bonbons: Int, piastres: Int)

import StateX._

object Dispenser {

  def regles: Action => Bonbonniere =>  Bonbonniere = (i: Action) => (s: Bonbonniere) => (i, s) match {
    case (_,Bonbonniere(_, 0, _)) => s
    case (Piastre, Bonbonniere(false, _, _)) => s
    case (Tourner, Bonbonniere(true, _, _)) => s
    case (Piastre, Bonbonniere(true, bonbons, piastres)) => Bonbonniere(bloque = false, bonbons, piastres + 1)
    case (Tourner, Bonbonniere(false, bonbons, piastres)) => Bonbonniere(bloque = true, bonbons - 1, piastres)
  }

  def simulateMachine(actions: List[Action]): StateX[Bonbonniere, (Int, Int)] =
    for {
      _ <- sequence(actions.map(modify[Bonbonniere] _ compose regles))
      s <- get
    } yield (s.piastres, s.bonbons)

  def main(args: Array[String]): Unit = {
    val s1 = Bonbonniere(bloque = true, 10, 10)
    assert(simulateMachine(List(Tourner)).run(s1) == ((10,10),s1))
    val s2 = Bonbonniere(bloque = true, 100, 0)
    assert(simulateMachine(List(Tourner)).run(s2)==((0,100),s2))
    assert(simulateMachine(List(Piastre)).run(Bonbonniere(bloque = true, 10, 5))._1._1==6)
    assert(simulateMachine(List(Tourner)).run(Bonbonniere(bloque = false, 5, 10))._1._2==4)
    val actions = List(Piastre,Tourner,Piastre,Tourner,Piastre,Tourner,Piastre,Tourner)
    assert(simulateMachine(actions).run(Bonbonniere(bloque = true, 5, 10))._1==(14,1))

  }

}
