package app.models.world.props

import app.models.world.{WObjectOps, WObjectStats, WObject}

trait PropOps extends WObjectOps
trait PropStats extends WObjectStats
trait PropCompanion extends PropOps with PropStats

/* A property in the world */
trait Prop extends WObject
