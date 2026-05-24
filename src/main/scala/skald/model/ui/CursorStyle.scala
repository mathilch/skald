package skald

enum CursorStyle(val ansiCode: String):
  case BlinkBlock extends CursorStyle("\u001b[1 q")
  case SolidBlock extends CursorStyle("\u001b[2 q")
  case BlinkUnderline extends CursorStyle("\u001b[3 q")
  case SolidUnderline extends CursorStyle("\u001b[4 q")
  case BlinkIBeam extends CursorStyle("\u001b[5 q")
  case SolidIBeam extends CursorStyle("\u001b[6 q")

object CursorStyle:
  def fromString(s: String): Option[CursorStyle] = s.toLowerCase match {
    case "block" => Some(BlinkBlock)
    case "solidblock" => Some(SolidBlock)
    case "ibeam" => Some(BlinkIBeam)
    case "solidibeam" => Some(SolidIBeam)
    case "underline" => Some(BlinkUnderline)
    case "solidunderline" => Some(SolidUnderline)
    case _ => None
  }
