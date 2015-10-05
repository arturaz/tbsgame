package app.algorithms.behaviour_trees

import akka.event.LoggingAdapter
import app.algorithms.behaviour_trees.AI.AI
import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
import app.models.game.world._
import app.models.game.{Player, Game}
import app.models.game.events.Evented

import scala.util.control.NonFatal
import scalaz._, Scalaz._
import implicits._

object BehaviourTree {
  sealed trait NodeResult[+V]
  object NodeResult {
    /* returned when a criterion has been met by a condition node or an action node has
       been completed successfully */
    case class Success[V](value: V) extends NodeResult[V]
    /* returned when a criterion has not been met by a condition node or an action node
       could not finish its execution for any reason */
    case class Failure(reason: String) extends NodeResult[Nothing]
    /* returned when some unexpected error happened in the tree, probably by a
       programming error (trying to verify an undefined variable). Its use depends on the
       final implementation of the leaf nodes */
    case class Error(error: String) extends NodeResult[Nothing]
    /* returned when an action node has been initialized but is still waiting the its
       resolution */
//    case object Running extends NodeResult[Nothing]

    @inline def success[A](value: A): NodeResult[A] = Success(value)
    @inline def failure(reason: String): NodeResult[Nothing] = Failure(reason)
    @inline def error(str: String): NodeResult[Nothing] = Error(str)
    @inline def fromOption[A](name: String)(optValue: Option[A]): NodeResult[A] =
      optValue.fold2(failure(name), success)
//    @inline def running: NodeResult = Running

    implicit object Typeclasses extends Monad[NodeResult] {
      override def bind[A, B](fa: NodeResult[A])(f: (A) => NodeResult[B]) = fa match {
        case Success(v) => f(v)
        case Failure(reason) => Failure(reason)
        case Error(err) => Error(err)
      }

      override def point[A](a: => A) = NodeResult.success(a)
    }
  }

  def const[S, V](v: V): BehaviourTree[S, V] =
    BehaviourTree { state => (state, NodeResult.success(v)) }

  def constNR[S, V](nr: NodeResult[V]): BehaviourTree[S, V] =
    BehaviourTree { state => (state, nr) }

  def get[S, V](f: S => V): BehaviourTree[S, V] =
    BehaviourTree { state => (state, NodeResult.success(f(state))) }

  def predicate[S](name: String)(f: S => Boolean): BehaviourTree[S, Unit] =
    BehaviourTree { state =>
      (
        state,
        if (f(state)) NodeResult.success(())
        else NodeResult.failure(s"$name predicate failed")
      )
    }
}

case class BehaviourTree[S, V](
  run: S => (S, NodeResult[V])
) extends AnyVal {
  def filter(name: V => String)(predicate: V => Boolean): BehaviourTree[S, V] =
    mapNR { value =>
      if (predicate(value)) NodeResult.success(value)
      else NodeResult.failure(s"filter failed: ${name(value)}")
    }

  /* Discard any value only keeping the node result. */
  def discard = map(_ => ())

  def map[V1](f: V => V1): BehaviourTree[S, V1] =
    mapNR { value => NodeResult.success(f(value)) }

  def mapNR[V1](f: V => NodeResult[V1]): BehaviourTree[S, V1] =
    flatMap(f andThen BehaviourTree.constNR)

  def flatMap[V1](f: V => BehaviourTree[S, V1]): BehaviourTree[S, V1] =
    mapSNRV {
      case (state, NodeResult.Success(value)) => f(value).run(state)
      case (state, NodeResult.Failure(reason)) => (state, NodeResult.failure(reason))
      case (state, NodeResult.Error(err)) => (state, NodeResult.error(err))
    }

  def orElse[V1 >: V](f: => BehaviourTree[S, V1]): BehaviourTree[S, V1] =
    mapSNRV {
      case (state, NodeResult.Failure(_) | NodeResult.Error(_)) => f.run(state)
      case orig @ (state, NodeResult.Success(value)) => orig
    }

  /* Turns successes into failures and vice versa. */
  def unary_! : BehaviourTree[S, Unit] = mapSNRV {
    case (state, NodeResult.Success(value)) => (state, NodeResult.failure("inverted"))
    case (state, NodeResult.Failure(_)) => (state, NodeResult.success(()))
    case (state, NodeResult.Error(err)) => (state, NodeResult.error(err))
  }

  /* Turns BehaviourTree that produces Boolean value into one which fails when produced
     value is false. */
  def predicate(implicit ev: V =:= Boolean): BehaviourTree[S, Unit] = mapNR {
    case true => NodeResult.success(())
    case false => NodeResult.failure("result was false")
  }

  @inline private[this] def mapSNRV[V1]
  (f: (S, NodeResult[V]) => (S, NodeResult[V1]))
  : BehaviourTree[S, V1] =
    BehaviourTree { s =>
      try run(s) match { case (state, result) => f(state, result) }
      catch { case NonFatal(e) => (s, NodeResult.error(e.toString)) }
    }
}

trait BehaviourTreeCompositeNodes {
  /* Runs all children sequentially until it finds one that does not return success. */
  def SequenceSameType[S, V](
    child: BehaviourTree[S, V], rest: BehaviourTree[S, V]*
  ): BehaviourTree[S, Vector[V]] = {
    def rec(
      current: BehaviourTree[S, Vector[V]],
      rest: Vector[BehaviourTree[S, V]]
    ): BehaviourTree[S, Vector[V]] = {
      current.flatMap { vect1 => rest match {
        case next +: tail => rec(next.map(vect1 :+ _), tail)
        case _ => BehaviourTree.const(vect1)
      } }
    }

    rec(child.map(Vector(_)), rest.toVector)
  }

  /* Runs all children sequentially until it finds one that does not return success. */
  def Sequence_[S](
    child: BehaviourTree[S, _], rest: BehaviourTree[S, _]*
  ): BehaviourTree[S, Unit] = {
    def rec(current: BehaviourTree[S, _], rest: Vector[BehaviourTree[S, _]])
    : BehaviourTree[S, Unit] = {
      current.flatMap { _ => rest match {
        case next +: tail => rec(next, tail)
        case _ => BehaviourTree.const(())
      } }
    }

    rec(child, rest.toVector)
  }

  /* Runs all children sequentially until if find one that does not return failure. */
  def Selector[S, V](
    child: BehaviourTree[S, V], rest: BehaviourTree[S, V]*
  ): BehaviourTree[S, V] = {
    def rec(
      current: BehaviourTree[S, V], rest: Vector[BehaviourTree[S, V]]
    ): BehaviourTree[S, V] = {
      current.orElse { rest match {
        case next +: tail => rec(next, tail)
        case _ => current
      }}
    }

    rec(child, rest.toVector)
  }
}

trait BehaviourTreeGameNodes {
  type St = Evented[Game]

  val Game = BehaviourTree.get { (_: St).value }
  val World = Game.map(_.world)
  val WorldObjects = World.map(_.objects)

  /* Returns how much resources does this player has */
  def GetResources(player: Player) = World.map(_.resources(player))

  /* Do we have enough resources to build something warpable? */
  def EnoughResourcesFor_?(player: Player, warpable: WarpableStats) =
    GetResources(player).
      filter(res => s"not enough resources ($res) for $warpable")(_ >= warpable.cost)

  /* Returns all world objects for player */
  def ObjectsFor(player: Player) = WorldObjects.map(_.forOwner(player))

  /* Succeeds if player owns units where predicate returns true, fails otherwise. */
  def OwnsWObject_?(player: Player)(predicate: PartialFunction[WObject, Boolean]) =
    ObjectsFor(player).map(_.exists(predicate.applyOrElse(_, (_: WObject) => false))).
    predicate

  /* Returns world objects for given player that satisfy given collector. */
  def WObjectsFor[A <: WObject](player: Player)(collector: PartialFunction[WObject, A]) =
    ObjectsFor(player).map(_.collectWO(collector))

  /* Returns free warp spots where new units can be warped in. */
  def FreeUnitWarpSpotsFor(player: Player) = World.map { world =>
    world.warpZoneMap.filterByOwner(player).filter(world.canWarp)
  }

  /* Succeeds with free random warp spot for player, fails otherwise. */
  def FreeRandomUnitWarpSpotFor(player: Player) =
    FreeUnitWarpSpotsFor(player).mapNR {
      _.toVector.random |> NodeResult.fromOption("no free warp spots")
    }

  /* Succeeds with optional warpable (which might get destroyed as a part of reaction) or
     errors out if warping in failed. */
  def WarpInFor[A <: Warpable](
    player: Player, what: WarpableCompanion.Of[A], position: Vect2
  )(implicit log: LoggingAdapter) = ???
//    BehaviourTree { (_: St).flatMap { game =>
//      game.warpW(player, position, what).fold(
//        err => Evented((game, NodeResult.failure(err))),
//        _.map(_.map2(NodeResult.success))
//      )
//    }.extractT1 }

  def WarpInInRandomPosition[A <: Warpable](
    player: Player, what: WarpableCompanion.Of[A]
  )(implicit log: LoggingAdapter) =
    FreeRandomUnitWarpSpotFor(player) flatMap (WarpInFor(player, DroneStats, _))
}

trait BehaviourTreeNodes extends BehaviourTreeCompositeNodes with BehaviourTreeGameNodes