package app.models

case class Attack(attackerRoll: Int, defenderRoll: Int) {
  def successful = attackerRoll >= defenderRoll

  override def toString = s"Atk[a: $attackerRoll vs d: $defenderRoll = $successful]"
}
