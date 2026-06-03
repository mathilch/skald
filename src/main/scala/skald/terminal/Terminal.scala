package skald

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.io.Reader
import scala.sys.process._

object Terminal {
  // Files.newBufferedReader pakker det hele pænt ind for dig
  val inputSource: Reader = Files.newBufferedReader(Paths.get("/dev/tty"), StandardCharsets.UTF_8)

  def setRaw(): Unit =
    Seq("sh", "-c", "stty -icanon -echo < /dev/tty").!

  def restore(): Unit =
    Seq("sh", "-c", "stty sane < /dev/tty").!
}
