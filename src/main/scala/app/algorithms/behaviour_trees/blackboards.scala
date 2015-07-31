//package app.algorithms.behaviour_trees
//
//import akka.event.LoggingAdapter
//import app.algorithms.Pathfinding.SearchRes
//import app.models.game.{Owner, Player, Game}
//import app.models.game.events.Evented
//import app.models.game.world._
//import monocle.{Lens, SimpleLens}
//
//trait GameBlackboard[A] {
//  def gameLens: SimpleLens[A, Evented[Game]]
//}
//
//trait PlayerContext[A] extends GameBlackboard[A] {
//  def playerLens: SimpleLens[A, Player]
//}
//
//case class FreeAsteroidData(asteroid: Asteroid, distanceFromEnemy: RadialDistance)
//trait FreeAsteroidContext[A] extends GameBlackboard[A] {
//  def freeAsteroidDataLens: SimpleLens[A, Option[FreeAsteroidData]]
//}
//
//trait FighterUnitContext[A] extends GameBlackboard[A] {
//  def unitLens: SimpleLens[A, Option[FUnit]]
//}
//
//trait AttackTargetBlackboard[A] extends GameBlackboard[A]  {
//  def attackTargetLens: SimpleLens[A, Option[SearchRes[OwnedObj]]]
//}
//
//case class ChildBlackboard[S, S1](
//  makeBlackboard: S => S1,
//  updateBlackboard: S1 => S
//)(_child: BehaviourTree[S1])
//extends BehaviourTree[S] {
//  override def run(state: S)(implicit log: LoggingAdapter) = {
//    val (result, newState) = _child.run(makeBlackboard(state))
//    (result, updateBlackboard(newState))
//  }
//}