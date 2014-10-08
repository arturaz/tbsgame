package app.models.game.world.props

import app.models.game.world.{WObjectOps, WObjectStats, WObject}

trait PropOps extends WObjectOps
trait PropStats extends WObjectStats
trait PropCompanion extends PropOps with PropStats

/* A property in the world */
trait Prop extends WObject
