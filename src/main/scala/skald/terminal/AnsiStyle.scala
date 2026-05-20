package skald 

enum Color(val code: String):
  case Reset   extends Color("\u001b[0m")
  case Bold    extends Color("\u001b[1m")
  case Green   extends Color("\u001b[32m")
  case Blue    extends Color("\u001b[34m")
  case Cyan    extends Color("\u001b[36m")
  case Magenta extends Color("\u001b[35m")

case class StyledText(text: String, styles: List[Color]):
  def render: String = 
    if (styles.isEmpty) text
    else s"${styles.map(_.code).mkString}$text${Color.Reset.code}"

object StyledText:
  def plain(text: String): StyledText = StyledText(text, Nil)  
