package skald

import java.io.{File, FileInputStream}
import scala.sys.process._

object RawTerminal {
  private val ttyFile = new File("/dev/tty")
  val inputSource = new FileInputStream(ttyFile)

  def setRaw(): Unit =
    // Brug sh -c for at være sikker på, at omdirigeringen bider
    Seq("sh", "-c", "stty -icanon -echo < /dev/tty").!

  def restore(): Unit =
    Seq("sh", "-c", "stty sane < /dev/tty").!
}
