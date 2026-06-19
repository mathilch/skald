package skald

enum Color(val ansiCode: String):
  case Red     extends Color("\u001b[31m")
  case Green   extends Color("\u001b[32m")
  case Blue    extends Color("\u001b[34m")
  case Yellow  extends Color("\u001b[33m")
  case Cyan    extends Color("\u001b[36m")
  case Magenta extends Color("\u001b[35m")
  case White   extends Color("\u001b[37m")
  case Black   extends Color("\u001b[30m")
  case Default extends Color("\u001b[39m")

enum Modifier(val ansiCode: String):
  case Bold      extends Modifier("\u001b[1m")
  case Underline extends Modifier("\u001b[4m")
  case Invert    extends Modifier("\u001b[7m")

object Color:
  def fromString(s: String): Option[Color] = s.toLowerCase match {
    case "red"      => Some(Red)
    case "green"    => Some(Green)
    case "blue"     => Some(Blue)
    case "yellow"   => Some(Yellow)
    case "cyan"     => Some(Cyan)
    case "magenta"  => Some(Magenta)
    case _          => None
  }

extension (s: String)
  def colorize(c: Color): String = s"${c.ansiCode}$s\u001b[0m"

case class Span(text: String, style: Style = Style())
case class Style(fg: Color = Color.Default, modifiers: Set[Modifier] = Set.empty)


