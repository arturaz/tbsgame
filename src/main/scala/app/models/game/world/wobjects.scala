package app.models.game.world

import app.models.game._
import app.models.game.world.props.PropImpl

/***************************[ PROPS ] ******************************/

/* Asteroids can be mined for resources */
case class Asteroid(
  position: Vect2, resources: Resources,
  id: WObject.Id=WObject.newId
) extends Prop

/* Rock is an immovable 1x1 obstacle */
case class Rock(
  position: Vect2, id: WObject.Id=WObject.newId
) extends Prop

/***************************[ BUILDINGS ] ******************************/


/***************************[ UNITS ] ******************************/


/***************************[ TRAITS ] ******************************/

/* World object */
sealed trait WObject extends WObjectImpl

/* A property in the world */
sealed trait Prop extends PropImpl

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

/* Can attack. */
sealed trait Fighter extends OwnedObj with FighterImpl

/* Can attack reactively not on owners turn. */
sealed trait ReactiveFighter extends Fighter with ReactiveFighterImpl

/* If destroyed this object should respawn with new owner and hp. */
sealed trait RespawnsOnDestruction extends OwnedObj