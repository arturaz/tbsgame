package app.models.game.world.maps

import java.io.{InputStream, File}

import app.models.game.Bot
import app.models.game.world.buildings._
import app.models.game.world._
import implicits._

import scala.util.{Random, Try}
import scala.xml.{XML, Node}
import scalaz.{ValidationNel, \/}
import scalaz.syntax.all._

/**
* Created by arturas on 2015-01-29.
*/
object TMXReader {
  private[this] def extractedIn(turns: Int) =
    ExtractorStats.turnStartExtracts  * Resources(turns) + ExtractorStats.cost

  val MaxResource = (extractedIn(15), extractedIn(30))
  val MidResource = (extractedIn(7), extractedIn(15))
  val MinResource = (extractedIn(3), extractedIn(10))

  private[this] val FlipHorizontal = 0x80000000
  private[this] val FlipVertical = 0x40000000
  private[this] val FlipDiagonal = 0x20000000
  private[this] val ClearFlags = ~(FlipHorizontal | FlipVertical | FlipDiagonal)

  sealed trait RenderOrder {
    def translate(idx: Int, layerSize: Vect2, worldBounds: Bounds): Vect2
  }
  object RenderOrder {
    case object RightDown extends RenderOrder {
      override def translate(idx: Int, layerSize: Vect2, worldBounds: Bounds) = {
        val xCoord = idx % layerSize.x
        val yCoord = idx / layerSize.x
        Vect2(worldBounds.start.x + xCoord, worldBounds.end.y - yCoord)
      }
    }
  }

  def read(file: File): String \/ GameMap = read(XML.loadFile(file))
  def read(is: InputStream): String \/ GameMap = read(XML.load(is))

  def read(root: Node): String \/ GameMap = {
    val worldBoundsE = parseBounds(root)
    val renderOrderE = parseStrAttr(root, "renderorder") {
      case "right-down" => RenderOrder.RightDown.rightZ
      case other => s"unknown render order: $other".leftZ
    }

    worldBoundsE.flatMap { worldBounds =>
      renderOrderE.flatMap { renderOrder =>
        (root \ "layer").foldLeft(
          GameMap.empty(worldBounds).rightZ[String]
        ) { (gameMapE, layer) =>
          gameMapE.flatMap { gameMap => parseLayer(renderOrder, layer, gameMap) }
        }
      }
    }
  }

  def parseLayer(
    renderOrder: RenderOrder, layer: Node, current: GameMap
  ): String \/ GameMap = {
    parseSize(layer).flatMap { layerSize =>
      (layer \ "data" \ "tile").view.zipWithIndex.foldLeft(current.rightZ[String]) {
        case (gameMapE, (tileNode, idx)) =>
          gameMapE.flatMap { gameMap => parseTile(
            renderOrder, layerSize, tileNode, idx, gameMap
          ) }
      }
    }
  }

  def parseTile(
    renderOrder: RenderOrder, layerSize: Vect2,
    tileNode: Node, idx: Int, current: GameMap
  ): String \/ GameMap = {
    val gidE =
      parseStrAttrT(tileNode, "gid")(_.parseLong.map(l => (l & ClearFlags).toInt))
    gidE.flatMap { gid =>
      if (gid === 0) current.rightZ
      else {
        val position = renderOrder.translate(idx, layerSize, current.bounds)
        update(gid, position, current)
      }
    }
  }

  def parseSize(node: Node) = {
    for {
      width <- parseStrAttrT(node, "width")(_.parseInt)
      height <- parseStrAttrT(node, "height")(_.parseInt)
    } yield Vect2(width, height)
  }

  def parseBounds(node: Node) = parseSize(node).map(size => Bounds(Vect2(0, 0), size))

  def parseStrAttr[A](node: Node, attr: String)(parser: String => String \/ A)
  : String \/ A =
    node.attribute(attr).map(_.head.text)
      .map(parser(_).leftMap(e => s"bad attribute '$attr' for node $node: $e"))
      .getOrElse(s"no $attr set".leftZ)

  def parseStrAttrT[A](node: Node, attr: String)(parser: String => Try[A])
  : String \/ A =
    parseStrAttr(node, attr)(s =>
      parser(s).fold(e => e.toString.leftZ, _.rightZ)
    )

  def update(
    gid: Int, position: Vect2, current: GameMap
  ): String \/ GameMap = {
    gid match {
      case 1 =>
        //current add Rock(position)
        current.rightZ
      case 2 | 3 | 4 => current add Asteroid(
        position, gid match {
          case 2 => resources(MaxResource)
          case 3 => resources(MidResource)
          case 4 => resources(MinResource)
        }
      )
      case 5 => current.addNpc(npc => Spawner(position, npc)).rightZ
      case 6 => current.addNpc(npc => VPTower(position, npc.team)).rightZ
      case 7 => current.addStarting(position).rightZ
    }
  }

  def resources(range: (Resources, Resources)) = Random.range(range._1, range._2)
}
