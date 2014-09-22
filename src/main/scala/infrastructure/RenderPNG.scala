package infrastructure

import java.awt.image.{BufferedImage, RenderedImage}
import java.awt.{Color, Font}
import java.io.File
import javax.imageio.ImageIO

import app.models.world._
import app.models.world.buildings.{Spawner, WarpGate}
import app.models.world.props.Asteroid
import app.models.world.units.Wasp
import app.models.{Owner, Player, Team}

/**
 * Created by arturas on 2014-09-11.
 */
object RenderPNG {
  val Colors = Seq(
    Color.black, Color.red, Color.black, Color.orange, Color.gray, Color.green,
    Color.blue, Color.cyan, Color.pink, Color.yellow, Color.magenta, new Color(123, 64, 21)
  )
  var teamColorsLeft = Colors
  var playerColorsLeft = Colors

  private[this] var colors = Map.empty[Owner, Color]
  def getColor(o: Owner) = colors.getOrElse(o, {
    val color = o match {
      case t: Team =>
        val c = teamColorsLeft.head
        teamColorsLeft = teamColorsLeft.tail
        c
      case p: Player =>
        val c = playerColorsLeft.head
        playerColorsLeft = playerColorsLeft.tail
        c
    }
    colors += o -> color
    color
  })

  def world(world: World, path: String): Unit = {
    val CellSize = 40
    val image = new BufferedImage(
      world.bounds.x.size * CellSize, world.bounds.y.size * CellSize,
      BufferedImage.TYPE_INT_RGB
    )
    val g = image.createGraphics()
    g.setColor(Color.white)
    g.fillRect(0, 0, image.getWidth, image.getHeight)

    def pngPos(v: Vect2) = (
      (v.x - world.bounds.x.start) * CellSize,
      (v.y - world.bounds.y.start) * CellSize
    )
    def pngSize(v: Vect2) = (v.x * CellSize, v.y * CellSize)

    def write(s: String, x: Int, y: Int) =
      g.drawString(s, x, y + CellSize)

    g.setColor(Color.black)
    val font = g.getFont
    g.setFont(new Font(font.getFontName, Font.PLAIN, 10))
    world.bounds.points.foreach { v =>
      val (x, y) = pngPos(v)
      write(s"${v.x},${v.y}", x, y)
    }
    g.setColor(Color.white)
    g.setFont(font)

    def draw(wo: WObject, color: Color, text: String = ""): Unit = {
      val oldColor = g.getColor

      val (teamColor, playerColor) = wo match {
        case fo: OwnedObj => (getColor(fo.owner.team), getColor(fo.owner))
        case _ => (color, color)
      }

      val size = wo match {
        case swo: SizedWObject => swo.stats.size
        case _ => Vect2.one
      }
      val ((x, y), (w, h)) = (pngPos(wo.position), pngSize(size))

      g.setColor(teamColor)
      g.fillRect(x, y, w / 2, h / 2)
      g.setColor(playerColor)
      g.fillRect(x + w / 2, y, w / 2, h / 2)
      g.setColor(color)
      g.fillRect(x, y + h / 2, w, h / 2)

      if (! text.isEmpty) {
        g.setColor(Color.WHITE)
        write(text, x, y)
      }
      g.setColor(oldColor)
    }

    world.objects.foreach {
      case wg: WarpGate =>
        draw(wg, Color.black, s"${wg.hp}/${wg.stats.maxHp}")
      case s: Spawner =>
        draw(s, new Color(115, 34, 34), s"${s.strength}: ${s.hp}/${s.stats.maxHp}")
      case asteroid: Asteroid =>
        draw(asteroid, Color.gray, asteroid.resources.toString)
      case wasp: Wasp =>
        draw(wasp, Color.red, "W")
    }

    save(image, s"$path.png")
  }

  def save(image: RenderedImage, path: String): Unit = {
    ImageIO.write(image, "png", new File(path))
  }
}
