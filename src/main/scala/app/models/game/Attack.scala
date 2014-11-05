package app.models.game

import app.models.game.world.{Fighter, HP, OwnedObj}
import implicits._

case class Attack(
  private val attackerRollPure: Int,
  private val kindMultiplier: Double,
  private val criticalMultiplier: Option[Double],
  defenderRoll: Int
) {
  @inline private[this] def criticalMult = criticalMultiplier.getOrElse(1d)
  val attackerRoll =
    (attackerRollPure * kindMultiplier).mapVal(_ * criticalMult).
    round.toInt
  def successful = attackerRoll > defenderRoll
  def damage = HP((attackerRoll - defenderRoll).max(0))

  /* None if destroyed, Some otherwise */
  def apply[A <: OwnedObj](obj: A): Option[A] =
    if (successful) obj.takeDamage(damage).map(_.asInstanceOf[A]) else Some(obj)

  override def toString =
    s"Atk[a: $attackerRollPure x [k: $kindMultiplier] x [c: $criticalMult] = ${
    attackerRoll} vs d: $defenderRoll = $successful]"
}

object Attack {
  def apply(fighter: Fighter, target: OwnedObj): Attack = {
    apply(
      fighter.companion.attack.random,
      fighter.companion.kind.multiplierAt(target.companion.kind),
      fighter.companion.critical.struck.opt(fighter.companion.criticalMultiplier),
      target.defense.random
    )
  }
}