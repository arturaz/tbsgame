package app.models.game.world

import app.models.game._

/* World object */
sealed trait WObject extends WObjectImpl

/* Object that belongs to some faction and not just a world prop */
sealed trait OwnedObj extends WObject with OwnedObjImpl
sealed trait TeamObj extends OwnedObj { val owner: Team }
sealed trait PlayerObj extends OwnedObj { val owner: Player }
sealed trait HumanObj extends PlayerObj { val owner: Human }
sealed trait BotObj extends PlayerObj { val owner: Bot }

/* Gives actions to its owner */
sealed trait GivingActions extends OwnedObj with GivingActionsImpl

/* Increases population cap to its owner. */
sealed trait GivingPopulation extends OwnedObj with GivingPopulationImpl

/* Gives victory points each turn */
sealed trait GivingVictoryPoints extends OwnedObj with GivingVictoryPointsImpl

/* Objects that can move. All such objects have 1x1 size. */
sealed trait Movable extends OwnedObj with MovableImpl

/* Objects that can do a special action. */
sealed trait SpecialAction extends OwnedObj with SpecialActionImpl

/* Objects that are bigger 1x1. Such objects cannot be moved. */
sealed trait SizedWObject extends WObject with SizedWObjectImpl

/* Has an internal counter that increases at each game turn. */
sealed trait TurnCounter extends WObject with TurnCounterImpl