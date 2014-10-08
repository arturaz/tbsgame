package app.models.game

import app.models.game.world.OwnedObj

/**
 * Created by arturas on 2014-10-08.
 */
case class Attack(attackerRoll: Int, defenderRoll: Int) {
  def successful = attackerRoll >= defenderRoll

  /* None if destroyed, Some otherwise */
  def apply[A <: OwnedObj](obj: A): Option[A] =
    if (successful) obj.takeDamage.map(_.asInstanceOf[A]) else Some(obj)

  override def toString = s"Atk[a: $attackerRoll vs d: $defenderRoll = $successful]"
}
