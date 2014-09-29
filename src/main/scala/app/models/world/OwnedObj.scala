package app.models.world

import app.models.game.events.Evented
import app.models.{Player, Bot, Human, Owner}
import implicits._

trait OwnedObjOps[Self] extends WObjectOps {
  def withNewHp(hp: Int)(self: Self): Self
}

trait OwnedObjStats extends WObjectStats {
  val maxHp: Int
  val visibility: Int
  val defense: Range
  /* If team has no critical objects it cannot do any more actions and
     loses the game. */
  val isCritical: Boolean = false

  protected def emptyRange = 0 until 0
}

trait OwnedObjCompanion[Self]
extends OwnedObjOps[Self] with OwnedObjStats

/* Object that belongs to some faction and not just a world prop */
trait OwnedObj extends WObject {
  type CompanionSelf <: OwnedObj
  type Companion <: OwnedObjOps[CompanionSelf] with OwnedObjStats

  val hp: Int
  val owner: Owner
  def isEnemy(o: OwnedObj) = owner.team != o.owner.team
  def isFriend(o: OwnedObj) = ! isEnemy(o)

  lazy val visibility = {
    val vis = companion.visibility
    Bounds(
      bounds.x.start - vis to bounds.x.end + vis,
      bounds.y.start - vis to bounds.y.end + vis
    )
  }
  def sees(obj: WObject) = visibility.intersects(obj.bounds)

  def takeDamage: Option[Self] =
    if (hp == 1) None else Some(self |> companion.withNewHp(hp - 1))

  def teamTurnStartedSelf(world: World): WorldSelfUpdate = Evented((world, self))
  final def teamTurnStarted(world: World): Evented[World] =
    teamTurnStartedSelf(world).map(_._1)

  def teamTurnFinishedSelf(world: World): WorldSelfUpdate = Evented((world, self))
  final def teamTurnFinished(world: World): Evented[World] =
    teamTurnFinishedSelf(world).map(_._1)
}

trait PlayerObj extends OwnedObj { val owner: Player }
trait HumanObj extends PlayerObj { val owner: Human }
trait BotObj extends PlayerObj { val owner: Bot }
