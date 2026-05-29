package skald

enum FunctionalOp(val func: String):
  case Filter extends FunctionalOp("filter")
  case Map    extends FunctionalOp("map")
  case Sort   extends FunctionalOp("sort")


object FunctionalOp:
  def fromString(s: String): Option[FunctionalOp] =
    values.find(_.func == s)
  def isFunctionalOp(s: String): Boolean =
    values.exists(_.func == s)
  

