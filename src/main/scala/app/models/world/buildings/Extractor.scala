package app.models.world.buildings

import app.models.Player
import app.models.game.events.{Evented, ResourceChangeEvt}
import app.models.world._
import app.models.world.props.Asteroid
import implicits._
import infrastructure.Log
import monocle.syntax._

object Extractor extends BuildingCompanion[Extractor] with WarpableCompanion[Extractor]
with SpecialActionCompanion {
  override val maxHp = 1
  override val size: Vect2 = Vect2.one
  override val warpTime: Int = 1
  override val cost: Int = 4
  /* How much resources does turn start extract? */
  val turnStartExtracts = 1
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

  override def setWarpState(newState: Int)(self: Extractor) =
    self.copy(warpState = newState)
  override def withNewHp(hp: Int)(self: Extractor) = self.copy(hp = hp)
}

case class Extractor(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: Int=Extractor.maxHp,
  warpState: Int=Extractor.InitialWarpState
) extends PlayerBuilding with Warpable with SpecialAction {
  override def companion = Extractor
  override protected def self = this
  override type Self = Extractor
  override type Companion = Extractor.type

  override def teamTurnStartedSelf(w: World) = {
    super.teamTurnStartedSelf(w).laterOptFlatMap { case (world, self) =>
      findAsteroid(world).fold(
        err => {
          Log.error(s"Can't find asteroid when team turn started for $this: $err")
          None
        },
        a => Some(turnStartExtractResources(world)(a).map((_, self)))
      )
    }
  }

  private[this] def findAsteroid(world: World): Either[String, Asteroid] = {
    world.find { case a: Asteroid if a.position == position => a }.fold2(
      s"Cannot find asteroid for $this!".left,
      _.right
    )
  }

  private[this] def extractResources(
    world: World, howMuch: Int
  )(asteroid: Asteroid): Evented[World] = {
    Evented(
      world.update(asteroid)(_ |-> Asteroid.resources modify (_ - howMuch)),
      Vector(
        ResourceChangeEvt(asteroid.id.left, -howMuch),
        ResourceChangeEvt(owner.id.right, howMuch)
      )
    )
  }

  private[this] def turnStartExtractResources(world: World) =
    extractResources(world, companion.turnStartExtracts) _

  def special(world: World) = {
    val extracts = companion.specialExtracts
    findAsteroid(world).right.flatMap { asteroid =>
      Either.cond(
        asteroid.resources >= extracts, asteroid,
        s"Not enough resources to extract from asteroid. Needed ${
          extracts}, had ${asteroid.resources}"
      )
    }.right.map(extractResources(world, extracts))
  }
}
