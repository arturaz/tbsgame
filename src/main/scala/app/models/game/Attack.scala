package app.models.game

import app.models.game.world.{Atk, Fighter, HP, OwnedObj}
import implicits._

case class Attack(
  private val attackerRollPure: Atk,
  private val kindMultiplier: Double,
  private val criticalMultiplier: Option[Double]
) {
  @inline private[this] def criticalMult = criticalMultiplier.getOrElse(1d)
  val attackerRoll =
    (attackerRollPure.value * kindMultiplier).mapVal(_ * criticalMult).
    round.toInt
  def successful = attackerRoll > 0
  def damage = HP(attackerRoll.max(0))

  /* None if destroyed, Some otherwise */
  def apply[A <: OwnedObj](obj: A): Option[A] =
    if (successful) obj.takeDamage(damage).map(_.asInstanceOf[A]) else Some(obj)

  override def toString =
    s"Atk[a: $attackerRollPure x [k: $kindMultiplier] x [c: $criticalMult] = ${
    attackerRoll}]"
}

object Attack {
  def apply(fighter: Fighter, target: OwnedObj): Attack = {
    apply(
      fighter.companion.randomAttack(fighter),
      fighter.companion.kind.multiplierAt(target.companion.kind),
      fighter.companion.critical.struck.opt(fighter.companion.criticalMultiplier)
    )
  }
}