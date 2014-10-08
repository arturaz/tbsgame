package app.models.game.world.buildings

import app.models.game.Player
import app.models.game.events.{Evented, ResourceChangeEvt}
import app.models.game.world._
import app.models.game.world.props.Asteroid
import implicits._
import infrastructure.Log
import monocle.syntax._

object Extractor extends BuildingCompanion[Extractor] with WarpableCompanion[Extractor]
with SpecialActionCompanion {
  override val maxHp = 1
  override val size: Vect2 = Vect2.one
  override val warpTime: Int = 1
  override val cost: Int = 4
  override val defense = 2 to 7
  /* How much resources does turn start extract? */
  val turnStartExtracts = 1
  /* How much resources does special action extract? */
  val specialExtracts = 2
  override val specialActionsNeeded: Int = 1

  override def warpWOReactionImpl(world: World, owner: Player, position: Vect2) = {
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
  type Self = Extractor
  def self = this
  override def companion = Extractor
  override type Companion = Extractor.type

  override def teamTurnStartedSelf(w: World) = {
    super.teamTurnStartedSelf(w).mapVal { upd => upd.flatMap { case (world, self) =>
      findAsteroid(world).fold(
        err => {
          Log.error(s"Can't find asteroid when team turn started for $this: $err")
          upd
        },
        asteroid => {
          if (asteroid.resources == 0) upd
          else turnStartExtractResources(world)(asteroid).fold(
            err => {
              Log.error(s"Error while extracting resources on turn start for $this: $err")
              upd
            },
            _.map((_, self))
          )
        }
      )
    } }
  }

  private[this] def findAsteroid(world: World): Either[String, Asteroid] = {
    world.find { case a: Asteroid if a.position == position => a }.fold2(
      s"Cannot find asteroid for $this!".left,
      _.right
    )
  }

  private[this] def extractResources(
    world: World, howMuch: Int
  )(asteroid: Asteroid): Either[String, Evented[World]] = {
    if (howMuch < 0) s"howMuch ($howMuch) has to be positive!".left
    else if (asteroid.resources == 0) s"No resources left in $asteroid!".left
    else {
      val res = asteroid.resources min howMuch

      val newSelf = asteroid |-> Asteroid.resources modify (_ - res)
      (
        world.updated(asteroid, newSelf) :+
        ResourceChangeEvt((world, asteroid).left, newSelf.resources)
      ).map(_.addResources(owner, howMuch)).extractFlatten
    }
  }

  private[this] def turnStartExtractResources(world: World) =
    extractResources(world, companion.turnStartExtracts) _

  def special(world: World) = {
    val extracts = companion.specialExtracts
    findAsteroid(world).right.flatMap(extractResources(world, extracts))
  }
}
