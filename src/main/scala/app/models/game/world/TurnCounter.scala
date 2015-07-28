package app.models.game.world

import akka.event.LoggingAdapter
import app.models.game.world.buildings.SpawnerOps
import utils.IntValueClass

import scala.language.implicitConversions

case class Turns(value: Int) extends IntValueClass[Turns] {
  def self(v: Int) = Turns(v)
}
object Turns {
  object TurnsNumeric extends IntValueClass.Numeric[Turns] {
    def fromInt(x: Int) = Turns(x)
  }
}

trait TurnCounterImpl extends WObjectImpl {
  val turns: Int
}

trait TurnCounterOps[+Self <: TurnCounter] {
  def self: Self
  def withTurns(turns: Int): Self
  def incTurns = withTurns(self.turns + 1)

  def gameTurnStarted(world: World)(implicit log: LoggingAdapter) = {
    val newSelf = incTurns
    world.updated(self, newSelf).map((_, newSelf))
  }
}

trait ToTurnCounterOps {
  implicit def toTurnCounterOps[A <: TurnCounter](a: A): TurnCounterOps[A] =
    (a match {
      case o: Spawner => SpawnerOps(o)
    }).asInstanceOf[TurnCounterOps[A]]
}