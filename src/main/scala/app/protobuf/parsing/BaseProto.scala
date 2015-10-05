package app.protobuf.parsing

import java.util.UUID

import app.models.game.world.Vect2
import netmsg._
import org.joda.time.DateTime
import utils.data.NonEmptyVector
import scalaz._, Scalaz._

import scala.language.{higherKinds, implicitConversions}

trait BaseProto {
  implicit def parseVect2(v: base.Vect2): Vect2 = Vect2(x = v.x, y = v.y)

  implicit def parseUUID(v: base.UUID): UUID =
    new UUID(v.mostSignificant, v.leastSignificant)

  def parse(timestamp: base.Timestamp): DateTime = new DateTime(timestamp.timestamp)

  def parsePath(
    pathList: Seq[base.Vect2]
  ): String \/ NonEmptyVector[Vect2] =
    NonEmptyVector.create(pathList.view.map(parseVect2).toVector)
    .toRightDisjunction("Can't create path from empty list!")
}
