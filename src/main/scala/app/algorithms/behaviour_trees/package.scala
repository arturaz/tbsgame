package app.algorithms

import app.models.game.Owner
import app.models.game.world.{WUnit, Fighter}
import monocle._

/**
 * Created by arturas on 15.2.1.
 */
package object behaviour_trees {
  type FUnit = WUnit with Fighter

//  implicit class StateExts[S](val state: S) extends AnyVal {
//    /**
//     * Allows using `state.get(_.attackTargetLens)` instead of
//     * `state.attackTargetLens.get(state)`.
//     **/
//    def get[A](getLens: S => SimpleLens[S, A]) =
//      getLens(state).get(state)
//
//    /**
//     * Allows using `state.set(_.attackTargetLens, None)` instead of
//     * `state.attackTargetLens.set(state, None)`.
//     **/
//    def set[A](getLens: S => SimpleLens[S, A], value: A) =
//      getLens(state).set(state, value)
//
//    /**
//     * Allows using `state.modify(_.attackTargetLens)(_ + 1)` instead of
//     * `state.attackTargetLens.modify(state, _ + 1)`.
//     **/
//    def modify[A](getLens: S => SimpleLens[S, A])(modify: A => A) =
//      getLens(state).modify(state, modify)
//
//    def evtGame(implicit ev: S <:< GameBlackboard[S]) = state.gameLens.get(state)
//    def game(implicit ev: S <:< GameBlackboard[S]) = state.evtGame.value
//    def world(implicit ev: S <:< GameBlackboard[S]) = state.game.world
//    def visibleWorld(owner: Owner)(implicit ev: S <:< GameBlackboard[S]) =
//      state.world.visibleBy(owner)
//
//    def player(implicit ev: S <:< PlayerContext[S]) = state.playerLens.get(state)
//  }
}
