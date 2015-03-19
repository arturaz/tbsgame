package app.models.game.world.props

import app.models.game.world.{Resources, Asteroid, WObjectStats}
import enumeratum.Enum

trait AsteroidImpl { self: Asteroid =>
  type Stats = AsteroidStats.type
  override val stats = AsteroidStats
}

sealed trait ExtractionSpeed {
  val resourcesPerTurn: Resources
}
object ExtractionSpeed extends Enum[ExtractionSpeed] {
  private[this] val baseExtractionRate = Resources(2)

  case object Slow extends ExtractionSpeed {
    override val resourcesPerTurn = baseExtractionRate
  }
  case object Medium extends ExtractionSpeed {
    override val resourcesPerTurn = baseExtractionRate + Resources(1)
  }
  case object Fast extends ExtractionSpeed {
    override val resourcesPerTurn = baseExtractionRate + Resources(2)
  }

  val values = findValues
}
object AsteroidStats extends WObjectStats
