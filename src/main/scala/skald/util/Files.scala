package skald 

import java.io.File
import java.nio.file.{Files => JFiles, Paths, StandardOpenOption}

object Files {

  def findEntriesInDirectory(dir: String, file: String): List[File] =
    Option(new File(dir).listFiles())
      .map(_.toList)
      .getOrElse(Nil)
      .filter(_.getName().startsWith(file))
      .sortBy(_.getName())

  def lcp(names: List[String]): String =
    if (names.isEmpty) ""
    else {
      val first = names.head
      val last = names.last

      first.zip(last).takeWhile(p => p._1 == p._2).map(_._1).mkString
    }

  def writeToFile(file: String, content: String): Unit = {
    JFiles.write(
      Paths.get(file), 
      content.getBytes, 
      StandardOpenOption.CREATE, 
      StandardOpenOption.TRUNCATE_EXISTING)
  }

  def readFromFile(file: String): List[String] = {
    import scala.jdk.CollectionConverters._

    val path = Paths.get(file)
    if (JFiles.exists(path)) JFiles.readAllLines(path).asScala.map(_.trim()).filter(_.nonEmpty).toList
    else List.empty[String]  
  }

  def appendToFile(file: String, content: String): Unit = {
    val lineWithSeperator = if content.nonEmpty then content + "\n" else ""

    JFiles.write(
      Paths.get(file), 
      content.getBytes, 
      StandardOpenOption.CREATE, 
      StandardOpenOption.APPEND)
  }

  def appendNewlineToFile(file: String, content: String): Unit = {
    val lineWithSeperator = content + "\n"

    JFiles.write(
      Paths.get(file), 
      lineWithSeperator.getBytes, 
      StandardOpenOption.CREATE, 
      StandardOpenOption.APPEND)
  }

}
