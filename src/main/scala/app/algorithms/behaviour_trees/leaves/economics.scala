//package app.algorithms.behaviour_trees.leaves
//
//import akka.event.LoggingAdapter
//import app.algorithms.behaviour_trees.BehaviourTree.NodeResult
//import app.algorithms.behaviour_trees._
//import app.models.game.Player
//import app.models.game.world._
//import implicits._
//import utils.data.NonEmptyVector
//
//trait PlayerPredicateCheck[S <: PlayerContext[S]]
//extends LeafNode[S]
//{
//  val predicate: Resources => Boolean
//  def getResources(world: World, player: Player)(implicit log: LoggingAdapter): Resources
//
//  override def run(state: S)(implicit log: LoggingAdapter) = {
//    val world = state.world
//    val player = state.player
//    (
//      if (predicate(getResources(world, player))) NodeResult.Success
//        else NodeResult.Failure,
//      state
//    )
//  }
//}
//
//case class IncomeCheck[
//  S <: PlayerContext[S]
//](predicate: Resources => Boolean)
//extends PlayerPredicateCheck[S]
//{
//  override def getResources(world: World, player: Player)(implicit log: LoggingAdapter) =
//    world.resourceExtractionNextTurn(player)
//}
//
//case class ResourcesCheck[
//  S <: PlayerContext[S]
//](predicate: Resources => Boolean)
//extends PlayerPredicateCheck[S]
//{
//  override def getResources(world: World, player: Player)(implicit log: LoggingAdapter) =
//    world.resources(player)
//}
//
//case class FindFreeAsteroidInWarpZone[
//  S <: PlayerContext[S] with FreeAsteroidContext[S]
//](
//  picker: NonEmptyVector[FreeAsteroidData] => Option[FreeAsteroidData]
//)
//extends LeafNode[S]
//{
//  override def run(state: S)(implicit log: LoggingAdapter) = {
//    val player = state.player
//    val world = state.visibleWorld(player)
//    val enemies = enemyObjects(world, player)
//    val unclaimedAsteroids = world.objects.collect {
//      case o: Asteroid
//        if world.objects.getCT[Extractor](o.position).isEmpty
//        && world.warpZoneMap.isVisible(player, o.position)
//      =>
//        FreeAsteroidData(o, distanceTo(enemies, o))
//    }
//    val picked = NonEmptyVector.create(unclaimedAsteroids).flatMap(picker)
//    picked.fold2(
//      (NodeResult.Failure, state),
//      data => (NodeResult.Success, state.freeAsteroidDataLens.set(state, Some(data)))
//    )
//  }
//}
//
//case class SpecialExtractData(
//  extractor: Extractor, asteroid: Asteroid, distanceFromEnemy: RadialDistance
//)
//case class SpecialExtract[
//  S <: PlayerContext[S]
//](
//  picker: NonEmptyVector[SpecialExtractData] => Option[SpecialExtractData]
//)
//extends LeafNode[S]
//{
//  override def run(state: S)(implicit log: LoggingAdapter): Run = {
//    val player = state.player
//    val world = state.visibleWorld(player)
//    val enemies = enemyObjects(world, player)
//    val extractors = world.objects.collect {
//      case o: Extractor if o.owner === player =>
//        o.findAsteroid(world).fold(
//          err => {
//            log.error(err)
//            None
//          },
//          asteroid => Some(SpecialExtractData(
//            o, asteroid, distanceTo(enemies, o)
//          ))
//        )
//    }.flatten
//    NonEmptyVector.create(extractors).flatMap(picker).fold2(
//      (NodeResult.failure, state),
//      { data =>
//        val actionsNeeded = data.extractor.stats.specialActionsNeeded
//        val game = state.game
//        game.states.get(player).fold2(
//          (NodeResult.Error(s"Cannot fetch game state for $player from ${game.states}"), state),
//          gameState => {
//            if (gameState.actions < actionsNeeded) return (NodeResult.failure, state)
//            val newGameState = gameState.copy(actions = gameState.actions - actionsNeeded)
//            data.extractor.special(world, player).fold(
//              err => (
//                NodeResult.Error(s"Error while special from ${data.extractor}: $err"),
//                state
//              ),
//              evtWorld => {
//                val newState = state.gameLens.modify(
//                  state,
//                  _.events ++: evtWorld.map { w =>
//                    game.copy(world = w, states = game.states + (player -> newGameState))
//                  }
//                )
//                (NodeResult.success, newState)
//              }
//            )
//          }
//        )
//      }
//    )
//  }
//}