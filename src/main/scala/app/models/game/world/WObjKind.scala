package app.models.game.world

/* < Light < Medium < Heavy < */
sealed trait WObjKind {
  /* Multiplier if `this` attacks `kind` */
  def multiplierAt(kind: WObjKind): Double
}
object WObjKind {
  private[this] val Mult = 3.0
  private[this] val InvMult = 1.0 / Mult

  case object Light extends WObjKind {
    def multiplierAt(kind: WObjKind) = kind match {
      case Light => 1
      case Medium => InvMult
      case Heavy => Mult
    }
  }
  case object Medium extends WObjKind {
    def multiplierAt(kind: WObjKind) = kind match {
      case Light => Mult
      case Medium => 1
      case Heavy => InvMult
    }
  }
  case object Heavy extends WObjKind {
    def multiplierAt(kind: WObjKind) = kind match {
      case Light => InvMult
      case Medium => Mult
      case Heavy => 1
    }
  }

  val All = Set(Light, Medium, Heavy)
}
