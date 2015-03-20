package app.models.game.world.buildings

import akka.event.LoggingAdapter
import app.models.game.events.{Evented, ResourceChangeEvt}
import app.models.game.world._
import app.models.game.{Percentage, Actions, Player, Population}
import implicits._

object ExtractorStats extends WBuildingStats with SpecialActionStats
with WarpableCompanion[Extractor]
{
  override val maxHp = HP(60)
  override val warpTime = WarpTime(1)
  override val cost = Resources(3)
  override val populationCost = Population(1)
  /* How much resource % does special action extract from asteroid? */
  val specialExtractsPercentage = Percentage(0.5f)
  /* How much fixed */
  val specialExtractsFixed = cost
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
}

case class ExtractorOps(self: Extractor) extends OwnedObjOps[Extractor]
with WarpableOps[Extractor] {
  override def withNewHp(hp: HP) = self.copy(hp = hp)
  override def setWarpState(newState: WarpTime) = self.copy(warpState = newState)
}

trait ExtractorImpl {
_: Extractor with BuildingImpl with WarpableImpl with SpecialActionImpl =>
  type Stats <: ExtractorStats.type

  def extractorTeamTurnStarted(world: World)(implicit log: LoggingAdapter): Evented[World] = {
    def orig = Evented(world)

    findAsteroid(world).fold(
      err => {
        log.error(s"Can't find asteroid when team turn started for $this: $err")
        orig
      },
      asteroid => {
        if (asteroid.resources.isZero) orig
        else turnStartExtractResources(world, asteroid).fold(
          err => {
            log.error(s"Error while extracting resources on turn start for $this: $err")
            orig
          },
          identity
        )
      }
    )
  }

  private[this] def findAsteroid(world: World): Either[String, Asteroid] = {
    world.objects.getCT[Asteroid](position).fold2(
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

      val newAsteroid = asteroid.copy(resources = asteroid.resources - res)
      (
        world.updated(asteroid, newAsteroid) :+
        ResourceChangeEvt((world, asteroid).left, newAsteroid.resources)
      ).map(_.addResources(owner, howMuch)).extractFlatten
    }
  }

  private[this] def turnStartExtractResources(world: World, asteroid: Asteroid) =
    extractResources(world, asteroid.extractionSpeed.resourcesPerTurn)(asteroid)

  override def specialImpl
  (world: World, invokedBy: Player)(implicit log: LoggingAdapter) = {
    findAsteroid(world).right.flatMap { asteroid =>
      val percentageResources = Resources(
        (asteroid.resources.value * stats.specialExtractsPercentage.value).round.toInt
      )
      val fixedResources = stats.specialExtractsFixed
      val resources = percentageResources + fixedResources
      (for {
        world <- world.addResources(owner, resources).right.get
        world <- world.removeEvt(this)
        world <- world.removeEvt(asteroid)
      } yield world).right
    }
  }
}