package app.models.game

import app.models.game.world.{HP, OwnedObj}

/**
 * Created by arturas on 2014-10-08.
 */
case class Attack(attackerRoll: Int, defenderRoll: Int) {
  def successful = attackerRoll > defenderRoll
  def damage = HP((attackerRoll - defenderRoll).max(0))

  /* None if destroyed, Some otherwise */
  def apply[A <: OwnedObj](obj: A): Option[A] =
    if (successful) obj.takeDamage(damage).map(_.asInstanceOf[A]) else Some(obj)

  override def toString = s"Atk[a: $attackerRoll vs d: $defenderRoll = $successful]"
}
