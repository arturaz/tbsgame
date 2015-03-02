package app.models.game.world

import helpers.{TestWObject, SizedTestWObjectStats, SizedTestWObject, AppTest}
import org.scalacheck.{Gen, Arbitrary}
import implicits._

import scalaz.\/-

class WorldObjsTest extends AppTest {
  implicit val arbWorldObjs = Arbitrary {
    Gen.containerOf[Vector, WObject](arbWObject.arbitrary).
      map(objs => WorldObjs(objs: _*).right_!)
  }

  "adding" - {
    "should create the id -> object mapping" in {
      forAll { (wObjs: Seq[WObject]) =>
        val \/-(wo) = WorldObjs(wObjs:_*)
        wo.objectsMap should === (wObjs.map { obj => obj.id -> obj }.toMap)
      }
    }

    "should save all the positions of an object" in {
      forAll { (wo: WObject) =>
        val objs = WorldObjs.empty.add_!(wo)

        val definedPoints = wo.bounds.points.collect {
          case p if objs.positionsMap.get(p).exists(s => s.contains(wo.id)) => p
        }.toSet
        definedPoints should === (wo.bounds.points.toSet)
      }
    }

    "should allow several objects being in the same position" in {
      forAll(arbPosition.arbitrary, sizeGen) { (position, size) =>
        val wo1 = SizedTestWObject(position, size)
        val wo2 = SizedTestWObject(position, size)
        val \/-(wo) = WorldObjs(wo1, wo2)
        val bothIds = Set(wo1.id, wo2.id)
        val expected = wo1.bounds.points.map(_ => bothIds).toSet
        wo1.bounds.points.map { p => wo.positionsMap(p) }.toSet should === (expected)
      }
    }

    "should fail if trying to add the same object twice" in {
      forAll { (wo: WObject) =>
        val objs = WorldObjs.empty.add_!(wo)
        objs.add(wo).isLeft should === (true)
      }
    }
  }

  "removing" - {
    "should fail when removing an object which does not exist" in {
      forAll { (objs: WorldObjs.All, wo: WObject) =>
        objs.remove(wo.id).isLeft should === (true)
      }
    }

    "should become an empty object after removing everything" in {
      forAll { (objs: WorldObjs.All) =>
        val actual = objs.foldLeft(objs) { case (fObjs, obj) => fObjs - obj.id }
        actual should === (WorldObjs.empty[WObject])
      }
    }

    "returns the same object after adding and removing same X objects" in {
      forAll { (initialObjs: WorldObjs.All, toBeRemoved: Seq[WObject]) =>
        val objs = initialObjs ++ toBeRemoved -- toBeRemoved.map(_.id)
        objs should === (initialObjs)
      }
    }
  }

  "updating" - {
    "should fail if the object that is being updated has a different id" in {
      forAll { (obj1: WObject, obj2: WObject) =>
        (WorldObjs.empty + obj1).update(obj1, obj2).isLeft should === (true)
      }
    }

    "should fail if the object that is being updated is not in the world" in {
      forAll { (objs: WorldObjs.All, obj: WObject) =>
        objs.update(obj, obj).isLeft should === (true)
      }
    }

    "should change the object in the objects map" in {
      forAll { (objs: WorldObjs.All, pos: Vect2) => whenever(objs.nonEmpty) {
        val obj = objs.head
        val newObj = TestWObject(pos, obj.id)
        objs.update_!(obj, newObj).objectsMap(obj.id) should === (newObj)
      } }
    }

    case class ChangePoints(
      oldObj: WObject, newObj: WObject, newObjs: WorldObjs.All
    ) {
      def id = oldObj.id
      def points(obj: WObject) = obj.bounds.points.toSet
      val oldPoints = points(oldObj)
      val newPoints = points(newObj)
      val removedPoints = oldPoints -- newPoints
    }
    def changePoints(f: ChangePoints => Unit): Unit = {
      forAll(
        arbWorldObjs.arbitrary, arbPosition.arbitrary, sizeGen
      ) { (objs, pos, size) => whenever(objs.nonEmpty) {
        val obj = objs.head
        val newObj = SizedTestWObject(pos, SizedTestWObjectStats(size), obj.id)
        val newObjs = objs.update_!(obj, newObj)

        f(ChangePoints(obj, newObj, newObjs))
      } }
    }

    "should remove old positions in the positions map" in {
      changePoints { data =>
        data.removedPoints.filter { p =>
          data.newObjs.positionsMap.get(p).exists(_.contains(data.id))
        } should === (Set.empty[Vect2])
      }
    }

    "should set new positions in the positions map" in {
      changePoints { data =>
        data.newPoints.filterNot { p =>
          data.newObjs.positionsMap.get(p).exists(_.contains(data.id))
        } should === (Set.empty[Vect2])
      }
    }
  }

  "filtering by positions" - {
    "should keep objects which are at least partially in those positions" in {
      forAll { (obj: WObject) =>
        val objs = WorldObjs.empty + obj
        val actual = obj.bounds.points.filter { p =>
          val filtered = objs.filterPartial(Seq(p))
          filtered != objs
        }.toSet
        actual should === (Set.empty[Vect2])
      }
    }

    "should remove objects which are not in given positions" in {
      forAll { (objs: WorldObjs.All) => whenever(objs.nonEmpty) {
        val objPoints = objs.head.bounds.points.toSet
        val filtered = objs.filterPartial(objPoints)
        val objsNotPartiallyInPoints = filtered.objectsMap.values.
          filter(_.bounds.points.toSet.intersect(objPoints).isEmpty).toVector
        objsNotPartiallyInPoints should === (Vector.empty[WObject])
      } }
    }

    "should remove object ids from positions for objects that are not partially in " +
    "those positions" in {
      forAll { (objs: WorldObjs.All) => whenever(objs.nonEmpty) {
        val objPoints = objs.head.bounds.points.toSet
        val nonIncludedObjects = objs.objectsMap.values.filter { obj =>
          obj.bounds.points.toSet.intersect(objPoints).isEmpty
        }.toVector
        val filtered = objs.filterPartial(objPoints)
        val actual = nonIncludedObjects.filter { obj =>
          obj.bounds.points.exists { p =>
            filtered.positionsMap.getOrElse(p, Set.empty).contains(obj.id)
          }
        }
        actual should === (Vector.empty[WObject])
      } }
    }
  }

  "filtering by predicate" - {
    "should be symmetrical in #filter and #filterNot" in {
      forAll { (objs: WorldObjs.All) => whenever(objs.nonEmpty) {
        val obj = objs.head
        objs.filter(_ == obj) should === (objs.filterNot(_ != obj))
      } }
    }

    "should remove objects properly" in {
      forAll { (objs: WorldObjs.All, toBeFiltered: Seq[WObject]) =>
        val added = objs ++ toBeFiltered
        added.filterNot(toBeFiltered.contains) should === (objs)
      }
    }
  }
}
