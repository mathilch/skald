package skald

import java.io.{File, FileInputStream, Reader}
import scala.sys.process._
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets

object RawTerminal {
  private val ttyFile = new File("/dev/tty")
  val inputSource: Reader = new InputStreamReader(new FileInputStream(ttyFile), StandardCharsets.UTF_8)

  def setRaw(): Unit =
    // Brug sh -c for at være sikker på, at omdirigeringen bider
    Seq("sh", "-c", "stty -icanon -echo < /dev/tty").!

  def restore(): Unit =
    Seq("sh", "-c", "stty sane < /dev/tty").!
}
