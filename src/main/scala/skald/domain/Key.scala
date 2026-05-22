package skald 

import java.io.Reader

sealed trait Key
case object UpArrow extends Key
case object DownArrow extends Key
case object LeftArrow extends Key 
case object RightArrow extends Key
case object Enter extends Key
case object Tab extends Key
case object Backspace extends Key
case object Escape extends Key
case object CtrlC extends Key
case object CtrlD extends Key
case class CharKey(c: Char) extends Key
case object Unknown extends Key

object KeyReader {
  def readKey(inputSource: Reader): Key = {
    inputSource.read() match {
      case -1 | 4 => CtrlD
      case 3      => CtrlC
      case 10 | 13 => Enter
      case 9      => Tab
      case 127    => Backspace
      case 27 => 
        if (inputSource.ready()) {
          val next1 = inputSource.read()
          val next2 = inputSource.read()
          (next1, next2) match {
            case (91, 65) => UpArrow
            case (91, 66) => DownArrow
            case (91, 67) => RightArrow
            case (91, 68) => LeftArrow
            case _        => Escape 
          }
        } else Escape 
        
      case c if c >= 32 => CharKey(c.toChar)
      case _ => Unknown
    }
  }
}
