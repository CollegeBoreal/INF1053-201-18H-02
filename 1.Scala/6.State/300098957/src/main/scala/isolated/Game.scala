package isolated

case class GameUnit(health: Int)
case class Game(score: Int, boss: GameUnit, party: List[GameUnit])

import scalaz.State._ // import StateFunctions (i.e. modify)

object Game {

  def strike : scalaz.State[Game, Unit] = modify[Game] { s =>
    s.copy(boss = s.boss.copy(health = s.boss.health - 10))
  }

  def fireBreath : scalaz.State[Game, Unit] = modify[Game] { s =>
    s.copy(party = s.party.map(u => u.copy(health = u.health - 5)).filter(_.health > 0))
  }

  /*
      Let's define a small "game" where some game units are fighting the boss (who is also a game unit).
      When the play is on we want to keep track of the game state, so let's define our "actions" in terms of a state monad:
      Let's hit the boss hard (strike) so he loses 10 from his health:
      And the boss can strike back! (fireBreath) When he does everyone in a party loses 5 health.
   */

  def main(args: Array[String]): Unit = {

    def play1 = for {
      _ <- strike
      _ <- fireBreath
      _ <- fireBreath
      _ <- strike
    } yield ()

    assert(play1.exec(Game(0, GameUnit(100), List(GameUnit(20), GameUnit(10))))==Game(0,GameUnit(80),List(GameUnit(10))))

    def play2 = for {
      _ <- strike
      _ <- fireBreath
      _ <- fireBreath
      _ <- strike
      _ <- fireBreath
    } yield ()

    assert(play2.exec(Game(0, GameUnit(100), List(GameUnit(20), GameUnit(10))))==Game(0,GameUnit(80),List(GameUnit(5))))

  }


}

