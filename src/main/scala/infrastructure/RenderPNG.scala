package infrastructure

import java.awt.{Font, Color}
import java.awt.image.{BufferedImage, RenderedImage}
import java.io.File
import javax.imageio.ImageIO

import app.models.Owner
import app.models.world.buildings.{Spawner, WarpGate}
import app.models.world.props.Asteroid
import app.models.world.units.Wasp
import app.models.world.{SizedWObject, Vect2, WObject, World}

import scala.util.Random

/**
 * Created by arturas on 2014-09-11.
 */
object RenderPNG {
  private[this] var colors = Map.empty[Owner, Color]
  def getColor(p: Owner) = colors.getOrElse(p, {
    val c = new Color(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256))
    colors += p -> c
    c
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

    g.setColor(Color.black)
    val font = g.getFont
    g.setFont(new Font(font.getFontName, Font.PLAIN, 10))
    world.bounds.points.foreach { v =>
      val (x, y) = pngPos(v)
      g.drawString(s"${v.x},${v.y}", x, y)
    }
    g.setColor(Color.white)
    g.setFont(font)

    def draw(wo: WObject, color: Color, text: String = ""): Unit = {
      val oldColor = g.getColor
      g.setColor(color)
      val size = wo match {
        case swo: SizedWObject => swo.stats.size
        case _ => Vect2.one
      }
      val ((x, y), (w, h)) = (pngPos(wo.position), pngSize(size))
      g.fillRect(x, y, w, h)
      if (! text.isEmpty) {
        g.setColor(Color.WHITE)
        g.drawString(text, x, y + CellSize)
      }
      g.setColor(oldColor)
    }

    world.objects.foreach {
      case wg: WarpGate =>
        draw(wg, Color.BLACK, s"${wg.hp}/${wg.stats.maxHp}")
      case s: Spawner =>
        draw(s, new Color(115, 34, 34), s"${s.hp}/${s.stats.maxHp}")
      case asteroid: Asteroid =>
        draw(asteroid, Color.GRAY, asteroid.resources.toString)
      case wasp: Wasp =>
        draw(wasp, getColor(wasp.owner), "W")
    }

    save(image, s"$path.png")
  }

  def save(image: RenderedImage, path: String): Unit = {
    ImageIO.write(image, "png", new File(path))
  }
}
