package infrastructure

import java.awt.Color
import java.awt.image.{RenderedImage, BufferedImage}
import java.io.File
import javax.imageio.ImageIO

import app.models.world.props.Asteroid
import app.models.world.units.Wasp
import app.models.world.{WObject, World}
import app.models.world.buildings.{Spawner, WarpGate}

/**
 * Created by arturas on 2014-09-11.
 */
object RenderPNG {
  def world(world: World, path: String): Unit = {
    val CellSize = 20
    val image = new BufferedImage(
      world.bounds.x.size * CellSize, world.bounds.y.size * CellSize,
      BufferedImage.TYPE_INT_RGB
    )
    val g = image.createGraphics()
    g.setColor(Color.white)
    g.fillRect(0, 0, image.getWidth, image.getHeight)

    def draw(wo: WObject, color: Color, text: String = ""): Unit = {
      val oldColor = g.getColor
      g.setColor(color)
      val (x, y, w, h) = (
        (wo.position.x - world.bounds.x.start) * CellSize,
        (wo.position.y - world.bounds.y.start) * CellSize,
        wo.stats.size.x * CellSize, wo.stats.size.y * CellSize
      )
      g.fillRect(x, y, w, h)
      if (! text.isEmpty) {
        g.setColor(Color.WHITE)
        g.drawString(text, x, y + CellSize)
      }
      g.setColor(oldColor)
    }

    world.objects.foreach {
      case wg: WarpGate =>
        draw(wg, Color.BLACK, "WG")
      case spawner: Spawner =>
        draw(spawner, new Color(115, 34, 34), ">-<")
      case asteroid: Asteroid =>
        draw(asteroid, Color.GRAY, asteroid.resources.toString)
      case wasp: Wasp =>
        draw(wasp, new Color(168, 118, 8), "W")
    }

    save(image, s"$path.png")
  }

  def save(image: RenderedImage, path: String): Unit = {
    ImageIO.write(image, "png", new File(path))
  }
}
