package app.models.game.world

import helpers.AppTest
import org.scalacheck.{Gen, Arbitrary}

/**
 * Created by arturas on 2015-01-15.
 */
class Vect2Test extends AppTest {
  "#isNextTo" - {
    "should return true if points are adjacent (with diagonals)" in {
      forAll(arbPosition.arbitrary, offsetGen) { (vect, offset) =>
        vect.isNextTo(vect + offset) should === (true)
      }
    }

    "should return false if points are the same" in {
      forAll(arbPosition.arbitrary) { vect =>
        vect.isNextTo(vect) should === (false)
      }
    }

    "should return false if points are not adjacent" in {
      forAll(arbPosition.arbitrary, offsetGen.map(_ * 2)) { (vect, offset) =>
        vect.isNextTo(vect + offset) should === (false)
      }
    }
  }
}
