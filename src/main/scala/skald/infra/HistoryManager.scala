package skald 

import java.nio.file.{Paths, Files => JFiles}

object HistoryManager {
  
  private var history = List.empty[String]
  private var historyBuffer = List.empty[String]

  def init(): Unit = {
    sys.env.get("HISTFILE") match {
      case Some(path) if JFiles.exists(Paths.get(path)) =>
        history = List.empty[String]
        historyBuffer = List.empty[String]
        readFromFile(path)
      case _ => ()
    }
  }

  def save(): Unit = {
    sys.env.get("HISTFILE") match {
      case Some(path) if JFiles.exists(Paths.get(path)) =>
        history.foreach(str => Files.appendNewlineToFile(path, str))
      case _ => ()
    }
  }


  def addCommand(cmd: String): Unit = 
    if (cmd.nonEmpty) {
      history = cmd :: history
      historyBuffer = cmd :: historyBuffer
    }
  
  def getAtIndex(idx: Int): String =
    history.lift(idx) match {
      case Some(s) => s
      case None => ""
    }
  
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
    Files.readFromFile(file).foreach(cmd => HistoryManager.addCommand(cmd))

  def writeToFile(file: String): Unit =
    history.foreach(cmd => Files.appendToFile(file, cmd))

  def appendToFile(file: String): Unit =
    historyBuffer.foreach(cmd => Files.appendNewlineToFile(file, cmd))
    historyBuffer = List.empty[String]
}


