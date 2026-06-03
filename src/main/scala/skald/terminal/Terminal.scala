package skald

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.io.Reader
import scala.sys.process._
import scala.util.Try

case class TerminalSize(columns: Int, rows: Int)

object Terminal {
  // Files.newBufferedReader pakker det hele pænt ind for dig
  val inputSource: Reader = Files.newBufferedReader(Paths.get("/dev/tty"), StandardCharsets.UTF_8)

  def setRaw(): Unit =
    Seq("sh", "-c", "stty -icanon -echo < /dev/tty").!

  def restore(): Unit =
    Seq("sh", "-c", "stty sane < /dev/tty").!

  def getSize(): TerminalSize = {
    Try {
      val output = Seq("sh", "-c", "stty size < /dev/tty").!!.trim
      val Array(rows, cols) = output.split(" ")
      TerminalSize(cols.toInt, rows.toInt)
    }.getOrElse(TerminalSize(80, 24)) 
  }
}
