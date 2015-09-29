package helpers

import app.models.game.world.WObject.Id
import app.models.game.world._
import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{FreeSpec, Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

case class TestWObject(
  position: Vect2, id: WObject.Id = WObject.newId
) extends WObjectTestRoot {
  type Stats = TestWObjectStats.type
  val stats = TestWObjectStats
}
case object TestWObjectStats extends WObjectStatsTestRoot

case class SizedTestWObjectStats(override val size: Vect2) extends WObjectStatsTestRoot with SizedWObjectStatsImpl

object SizedTestWObject {
  def apply(position: Vect2, size: Vect2): SizedTestWObject =
    apply(position, SizedTestWObjectStats(size))
}
case class SizedTestWObject(
  position: Vect2, stats: SizedTestWObjectStats, id: WObject.Id = WObject.newId
) extends WObjectTestRoot {
  type Stats = SizedTestWObjectStats
}

class AppTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks
with TypeCheckedTripleEquals
{
  implicit val arbPosition = Arbitrary(for {
    x <- Gen.chooseNum(-100000, 100000)
    y <- Gen.chooseNum(-100000, 100000)
  } yield Vect2(x, y))

  val sizeGen = Gen.sized { size =>
    val realSize = 1 max size min 20
    for {
      x <- Gen.chooseNum(1, realSize)
      y <- Gen.chooseNum(1, realSize)
    } yield Vect2(x, y)
  }

  val wObjectGen = arbPosition.arbitrary.map(TestWObject.apply(_))
  val sizedWObjectGen = for {
    position <- arbPosition.arbitrary
    size <- sizeGen
  } yield SizedTestWObject(position, size)
  implicit val arbWObject: Arbitrary[WObject] =
    Arbitrary(Gen.oneOf(wObjectGen, sizedWObjectGen))

  val offsetGen = {
    val offset = Gen.chooseNum(-1, 1)
    val offsetWOZero = Gen.oneOf(-1, 1)
    offset.flatMap { x =>
      (if (x == 0) offsetWOZero else offset).map { y => Vect2(x, y) }
    }
  }
}
