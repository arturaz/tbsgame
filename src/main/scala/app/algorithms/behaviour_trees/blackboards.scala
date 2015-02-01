package app.algorithms.behaviour_trees

import app.algorithms.Pathfinding.SearchRes
import app.models.game.Game
import app.models.game.events.Evented
import app.models.game.world.{World, OwnedObj}
import monocle.SimpleLens

trait GameBlackboard[A] {
  def gameLens: SimpleLens[A, Evented[Game]]
}

trait WorldBlackboard[A] {
  def worldLens: SimpleLens[A, Evented[World]]
}

trait FighterUnitContext[A] extends GameBlackboard[A] {
  def unitLens: SimpleLens[A, Option[FUnit]]
}

trait AttackTargetBlackboard[A] extends GameBlackboard[A]  {
  def attackTargetLens: SimpleLens[A, Option[SearchRes[OwnedObj]]]
}
