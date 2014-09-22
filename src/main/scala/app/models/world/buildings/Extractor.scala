package app.models.world.buildings

import app.models.Player
import app.models.game.events.ResourceChangeEvt
import app.models.world._
import app.models.world.props.Asteroid
import implicits._
import monocle.syntax._

object Extractor extends BuildingStats with WarpableStats[Extractor]
with SpecialActionStats {
  override val maxHp = 1
  override val size: Vect2 = Vect2.one
  override val warpTime: Int = 1
  override val cost: Int = 4
  /* How much resources does special action extract? */
  val specialExtracts = 2
  override val specialActionsNeeded: Int = 1

  override def warp(world: World, owner: Player, position: Vect2) = {
    val b = bounds(position)
    val objects = world.objects.filter(_.bounds == b)
    val isAsteroid = objects.exists(_.isInstanceOf[Asteroid])
    if (! isAsteroid || objects.size != 1)
      Left(s"Expected $b to only have asteroid, but there were $objects")
    else
      Right(Extractor(position, owner))
  }
}

case class Extractor(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: Int=Extractor.maxHp,
  warpState: Int=Extractor.InitialWarpState
) extends PlayerBuilding with Warpable with SpecialAction {
  override def stats = Extractor

  override protected def advanceWarpState(self: Self, newState: Int) =
    copy(warpState = newState)
  override protected def self = this
  override type Self = Extractor
  override type Stats = Extractor.type
  override protected def withNewHp(hp: Int) = copy(hp = hp)

  private[this] def findAsteroid(world: World): Either[String, Asteroid] = {
    world.find { case a: Asteroid if a.position == position => a }.fold2(
      s"Cannot find asteroid for $this!".left,
      asteroid => Either.cond(
        asteroid.resources >= stats.specialExtracts, asteroid,
        s"Not enough resources to extract from asteroid. Needed ${
          stats.specialExtracts}, had ${asteroid.resources}"
      )
    )
  }

  def special(world: World) = findAsteroid(world).right.map { asteroid =>
    val extracts = stats.specialExtracts
    (
      world.update(asteroid)(_ |-> Asteroid.resources modify (_ - extracts)),
      Vector(
        ResourceChangeEvt(asteroid.id.left, -extracts),
        ResourceChangeEvt(owner.id.right, extracts)
      )
    )
  }
}
