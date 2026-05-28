package skald 

import java.nio.file.{Paths, Files => JFiles}
import scala.util.Properties
import Result._

object HistoryManager {
  
  private var history = List.empty[String]
  private var historyBuffer = List.empty[String]

  val home = System.getProperty("user.home")
  private val historyPath: String = sys.env.getOrElse("HISTFILE", s"$home/.skald_history")

  def init(): Unit = {
    val lines = readFromFile(historyPath)
    history = lines.reverse
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
      s"    ${formatIdx(idx)}  $cmd"
    }.mkString("\n") + "\n"

  def showHistory(n: Int): String =
    history
      .reverse
      .zipWithIndex
      .takeRight(n)
      .map { (cmd, idx) =>
        s"    ${formatIdx(idx)}  $cmd"
      }
      .mkString("\n") + "\n"

  def readFromFile(file: String): List[String] =
    Files.readFromFile(file) match {
      case Success(lines) => lines.map(_.trim()).filter(_.nonEmpty).toList
      case Fail(err)      => List.empty[String]
    }

  def writeToFile(file: String): Unit =
    Files.writeToFile(file, history.reverse.iterator)

  def appendToFile(file: String): Unit =
    if (historyBuffer.nonEmpty) {
      Files.appendToFile(file, historyBuffer.reverse.iterator)
      historyBuffer = List.empty[String]
    }

  private def formatIdx(idx: Int): String =
    val totalCommands = history.size
    val width = totalCommands.toString.length

    s"%${width}d".format(idx + 1)


  def getSuggestion(currentInput: String): Option[String] =
    if (currentInput.isEmpty()) None
    else history.find(_.startsWith(currentInput))
}


