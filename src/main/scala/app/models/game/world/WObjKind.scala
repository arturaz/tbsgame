package app.models.game.world

sealed trait WObjKind
object WObjKind extends enumeratum.Enum[WObjKind] {
  case object Light extends WObjKind
  def light: WObjKind = Light

  case object Medium extends WObjKind
  def medium: WObjKind = Medium

  case object Armored extends WObjKind
  def armored: WObjKind = Armored

  case object Structure extends WObjKind
  def structure: WObjKind = Structure

  val values = findValues
}
