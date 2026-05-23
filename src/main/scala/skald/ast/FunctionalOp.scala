package skald

enum FunctionalOp:
  case Filter, Map, Sort


object FunctionalOp:
  def fromString(s: String): Option[FunctionalOp] = s match {
    case "filter" => Some(Filter)
    case "map"    => Some(Map)
    case "sort"   => Some(Sort)
    //case "sort"   => Some(Sort)
    case _        => None
  }

