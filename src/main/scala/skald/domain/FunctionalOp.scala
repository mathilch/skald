package skald

enum FunctionalOp:
  case Filter, Map


object FunctionalOp:
  def fromString(s: String): Option[FunctionalOp] = s match {
    case "filter" => Some(Filter)
    case "map"    => Some(Map)
    //case "sort"   => Some(Sort)
    case _        => None
  }

