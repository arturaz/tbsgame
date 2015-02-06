package app.models.game.world

import app.models.game._

/* World object */
sealed trait WObject {
  val id: WObject.Id
  val position: Vect2
  val stats: WObjectStats

  lazy val bounds = WObject.bounds(position)
}

/* Object that belongs to some faction and not just a world prop */
sealed trait OwnedObj extends WObject {
  val stats: OwnedObjStats
  val hp: HP
  val owner: Owner

  def maxHp = stats.maxHp
  def isWarpingIn = false
  def isWarpedIn = ! isWarpingIn
  def isEnemy(o: OwnedObj) = owner.team =/= o.owner.team
  def isFriend(o: OwnedObj) = ! isEnemy(o)
  def destroyReward = Option.empty[Resources]

  lazy val visibility = stats.visibility.extend(bounds)
  def sees(obj: WObject) = visibility.intersects(obj.bounds)

  lazy val warpZone =
    if (stats.warpGiven.isNotZero) Some(stats.warpGiven.extend(bounds))
    else None
}
sealed trait TeamObj extends OwnedObj { val owner: Team }
sealed trait PlayerObj extends OwnedObj { val owner: Player }
sealed trait HumanObj extends PlayerObj { val owner: Human }
sealed trait BotObj extends PlayerObj { val owner: Bot }

/* Gives actions to its owner */
sealed trait GivingActions extends OwnedObj {
  val stats: GivingActionsStats
}

/* Increases population cap to its owner. */
sealed trait GivingPopulation extends OwnedObj {
  val stats: GivingPopulationStats

  def populationGiven = if (isWarpedIn) stats.populationGiven else Population(0)
}

/* Gives victory points each turn */
sealed trait GivingVictoryPoints extends OwnedObj {
  val stats: GivingVictoryPointsStats
}

/* Objects that can move. All such objects have 1x1 size. */
sealed trait Movable extends OwnedObj with Mobility[Mobility.Movable.type] {
  val movementLeft: Movement
  val stats: MovableWObjectStats
}