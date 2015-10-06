package app.models.game

import app.models.game.world._
import implicits._

case class Attack(
  private val attackerBaseRoll: Atk,
  private val criticalMultiplier: Option[Double]
) {
  @inline private[this] def criticalMult = criticalMultiplier.getOrElse(1d)
  val attackerRoll = (attackerBaseRoll.value * criticalMult).round.toInt
  def successful = attackerRoll > 0
  def damage = HP(attackerRoll max 0)

  /* None if destroyed, Some otherwise */
  def apply[A <: OwnedObj](obj: A): Option[A] =
    if (successful) obj.takeDamage(damage) else Some(obj)

  override def toString =
    s"Atk[a: $attackerRoll x [c: $criticalMult] = $attackerRoll]"
}

object Attack {
  def apply(fighter: Fighter, target: OwnedObj): Attack = {
    apply(
      fighter.randomAttackTo(target.stats.kind),
      fighter.stats.critical.struck.opt(fighter.stats.criticalMultiplier)
    )
  }
}