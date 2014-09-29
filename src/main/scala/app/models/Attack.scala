package app.models

import app.models.world.OwnedObj

case class Attack(attackerRoll: Int, defenderRoll: Int) {
  def successful = attackerRoll >= defenderRoll

  /* None if destroyed, Some otherwise */
  def apply[A <: OwnedObj](obj: A): Option[A] =
    if (successful) obj.takeDamage else Some(obj)

  override def toString = s"Atk[a: $attackerRoll vs d: $defenderRoll = $successful]"
}
