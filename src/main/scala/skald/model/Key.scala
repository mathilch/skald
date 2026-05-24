package skald 

import java.io.Reader

enum Key:
  case UpArrow, DownArrow, LeftArrow, RightArrow
  case Enter, Tab, Backspace, Escape, CtrlC, CtrlD, Unknown
  case End, Home
  case CharKey(c: Char)

object KeyReader {
  import Key._

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
            case (91, 70) => End
            //case (91, 72) => Home unused for now
            case _        => Escape 
          }
        } else Escape 
        
      case c if c >= 32 => CharKey(c.toChar)
      case _ => Unknown
    }
  }
}
