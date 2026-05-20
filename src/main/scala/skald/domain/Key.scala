package skald 

sealed trait Key
case object UpArrow extends Key
case object DownArrow extends Key
case object Enter extends Key
case object Tab extends Key
case object Backspace extends Key
case object Escape extends Key
case object CtrlC extends Key
case object CtrlD extends Key
case class CharKey(c: Char) extends Key
case object Unknown extends Key

object KeyReader {
  def readKey(inputSource: java.io.InputStream): Key = {
    inputSource.read() match {
      case -1 | 4 => CtrlD
      case 3      => CtrlC
      case 10 | 13 => Enter
      case 9      => Tab
      case 127    => Backspace
      case 27 => 
        if (inputSource.available() > 0) {
          val next1 = inputSource.read()
          val next2 = inputSource.read()
          (next1, next2) match {
            case (91, 65) => UpArrow
            case (91, 66) => DownArrow
            case _        => Escape 
          }
        } else Escape 
        
      case c if c >= 32 && c <= 126 => CharKey(c.toChar)
      case _ => Unknown
    }
  }
}
