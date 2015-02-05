package app.models.game.world.maps

import app.models.game.Bot
import app.models.game.world.buildings.{Extractor, Spawner, VPTower}
import app.models.game.world.props.{Asteroid, Rock}
import app.models.game.world.{Resources, Vect2}
import implicits._

import scala.util.{Random, Try}
import scala.xml.Node
import scalaz.\/
import scalaz.syntax.all._

/**
 * Created by arturas on 2015-01-29.
 */
object TMXReader {
  private[this] def extractedIn(turns: Int) = Extractor.turnStartExtracts * Resources(turns)

  val MaxResource = (extractedIn(15), extractedIn(30))
  val MidResource = (extractedIn(7), extractedIn(15))
  val MinResource = (extractedIn(3), extractedIn(10))

  private[this] val FlipHorizontal = 0x80000000
  private[this] val FlipVertical = 0x40000000
  private[this] val FlipDiagonal = 0x20000000
  private[this] val ClearFlags = ~(FlipHorizontal | FlipVertical | FlipDiagonal)

  sealed trait RenderOrder
  object RenderOrder {
    case object RightDown extends RenderOrder
  }

//  def read(file: File): ValidationNel[String, World] = read(XML.loadFile(file))
//
//  def read(root: Node): ValidationNel[String, World] = {
//    val width = parseStrAttr(root, "width")(_.parseInt)
//    val height = parseStrAttr(root, "height")(_.parseInt)
//    val renderOrder = parseStrAttr(root, "renderorder") {
//      case "right-down" => Success(RenderOrder.RightDown)
//      case other => Failure(new Throwable(s"unknown render order: $other"))
//    }
//  }

  def readAttr(node: Node, attr: String) =
    parseStrAttr(node, attr)(util.Success(_))

  def parseStrAttr[A](node: Node, attr: String)(parser: String => Try[A]) =
    node.attribute(attr).map(_.head.text)
      .map(parser(_).fold(e => s"bad $attr: $e".failureNel, _.successNel))
      .getOrElse(s"no $attr set".failureNel)

  def update(
    gid: Int, position: Vect2, npcOwner: Bot, current: GameMap
  ): String \/ GameMap = {
    gid match {
      case 1 => current add Rock(position)
      case 2 | 3 | 4 => current add Asteroid(
        position, gid match {
          case 2 => resources(MaxResource)
          case 3 => resources(MidResource)
          case 4 => resources(MinResource)
        }
      )
      // TODO? add property support
      case 5 => current add Spawner(position, npcOwner)
      case 6 => current add VPTower(position, npcOwner.team)
//      case 7 => current.addStarting(position).r
    }
  }

  def resources(range: (Resources, Resources)) = Random.range(range._1, range._2)
}
