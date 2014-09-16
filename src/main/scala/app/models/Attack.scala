package app.models

case class Attack(attackerRoll: Int, defenderRoll: Int) {
  def successful = attackerRoll >= defenderRoll
}
