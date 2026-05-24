package skald

enum Color(val ansiCode: String):
  case Red      extends Color("\u001b[31m")
  case Green    extends Color("\u001b[32m")
  case Blue     extends Color("\u001b[34m")
  case Yellow   extends Color("\u001b[33m")
  case Bold     extends Color("\u001b[1m")
  case Cyan     extends Color("\u001b[36m")
  case Magenta  extends Color("\u001b[35m")
  case Reset    extends Color("\u001b[0m")

object Color:
  def fromString(s: String): Option[Color] = s.toLowerCase match {
    case "red"      => Some(Red)
    case "green"    => Some(Green)
    case "blue"     => Some(Blue)
    case "yellow"   => Some(Yellow)
    case "bold"     => Some(Bold)
    case "cyan"     => Some(Cyan)
    case "magenta"  => Some(Magenta)
    case "reset"    => Some(Reset)
    case _          => None
  }




