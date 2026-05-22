package skald 

import java.nio.file.{Paths, Files => JFiles}
import scala.util.Properties

object HistoryManager {
  
  private var history = List.empty[String]
  private var historyBuffer = List.empty[String]

  val home = System.getProperty("user.home")
  private val historyPath: String = sys.env.getOrElse("HISTFILE", s"$home/.skald_history")

  def init(): Unit = {
    val lines = Files.readFromFile(historyPath)
    history = lines.foldLeft(List.empty[String])((acc, elem) => elem :: acc)
  }

  def save(): Unit = 
    if (historyBuffer.nonEmpty) appendToFile(historyPath)
  

  def addCommand(cmd: String): Unit = 
    if (cmd.nonEmpty) {
      history = cmd :: history
      historyBuffer = cmd :: historyBuffer
    }
  
  def getAtIndex(idx: Int): String =
    history.lift(idx).getOrElse("")
  
  def size: Int =
    history.size

  def showHistory(): String = 
    history.reverse.zipWithIndex.map { case (cmd, idx) =>
      s"    ${idx + 1}  $cmd"
    }.mkString("\n") + "\n"

  def showHistory(n: Int): String =
    history
      .reverse
      .zipWithIndex
      .takeRight(n)
      .map { (cmd, idx) =>
        s"    ${idx + 1}  $cmd"
      }
      .mkString("\n") + "\n"

  def readFromFile(file: String): Unit =
    Files.readFromFile(file).foreach(cmd => history = cmd :: history)

  def writeToFile(file: String): Unit =
    history.foreach(cmd => Files.appendToFile(file, cmd))

  def appendToFile(file: String): Unit =
    historyBuffer.foreach(cmd => Files.appendNewlineToFile(file, cmd))
    historyBuffer = List.empty[String]
}


