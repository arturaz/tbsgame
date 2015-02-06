package app.models.game.world.buildings

import akka.event.LoggingAdapter
import app.models.game.events.{Evented, ResourceChangeEvt}
import app.models.game.world._
import app.models.game.world.props.Asteroid
import app.models.game.{Population, Actions, Player}
import implicits._
import monocle.syntax._

object Extractor extends WBuildingCompanion[Extractor]
with SpecialActionCompanion[Extractor] {
  override val maxHp = HP(115)
  override val warpTime = WarpTime(1)
  override val cost = Resources(10)
  override val populationCost = Population(1)
  /* How much resources does turn start extract? */
  val turnStartExtracts = Resources(1)
  /* How much resources does special action extract? */
  val specialExtracts = Resources(3)
  val specialCollapseResources = cost
  override val specialActionsNeeded = Actions(1)
  override val kind = WObjKind.Medium

  override def warpWOReactionImpl(world: World, owner: Player, position: Vect2) = {
    val b = bounds(position)
    val objects = world.objects.objectsIn(b)
    val isAsteroid = objects.exists(_.isInstanceOf[Asteroid])
    if (! isAsteroid || objects.size =/= 1)
      Left(s"Warping in: expected $b to only have asteroid, but there were $objects")
    else
      Right(Extractor(position, owner))
  }

  override def setWarpState(newState: WarpTime)(self: Extractor) =
    self.copy(warpState = newState)

  case class Ops(self: Extractor) extends OwnedObjOps[Extractor] {
    override def withNewHp(hp: HP) = self.copy(hp = hp)
  }
}

case class Extractor(
  position: Vect2, owner: Player,
  id: WObject.Id=WObject.newId, hp: HP=Extractor.maxHp,
  warpState: WarpTime=Extractor.InitialWarpState
) extends PlayerBuilding with WBuilding with SpecialAction {
  type Self = Extractor
  def self = this
  override def companion = Extractor
  override type Companion = Extractor.type

  override def teamTurnStartedSelf(w: World)(implicit log: LoggingAdapter) = {
    super.teamTurnStartedSelf(w).mapVal { upd => upd.flatMap {
    case orig @ (world, self) =>
      findAsteroid(world).fold(
        err => {
          log.error(s"Can't find asteroid when team turn started for $this: $err")
          Evented(orig)
        },
        asteroid => {
          if (asteroid.resources.isZero) Evented(orig)
          else turnStartExtractResources(world)(asteroid).fold(
            err => {
              log.error(s"Error while extracting resources on turn start for $this: $err")
              upd
            },
            _.map((_, self))
          )
        }
      )
    } }
  }

  private[this] def findAsteroid(world: World): Either[String, Asteroid] = {
    world.find { case a: Asteroid if a.position === position => a }.fold2(
      s"Cannot find asteroid for $this!".left,
      _.right
    )
  }

  private[this] def extractResources(
    world: World, howMuch: Resources
  )(asteroid: Asteroid): Either[String, Evented[World]] = {
    if (howMuch < Resources(0)) s"howMuch ($howMuch) has to be positive!".left
    else if (asteroid.resources.isZero) s"No resources left in $asteroid!".left
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

  override def specialImpl(world: World) = {
    val extracts = companion.specialExtracts
    findAsteroid(world).right.flatMap { asteroid =>
      if (asteroid.resources.isZero) (for {
        world <- world.addResources(owner, companion.specialCollapseResources).right.get
        world <- world.removeEvt(this)
        world <- world.removeEvt(asteroid)
      } yield world).right
      else extractResources(world, extracts)(asteroid)
    }
  }
}
