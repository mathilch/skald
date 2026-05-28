package skald 

import java.nio.file.{Files => JFiles, Paths, Path, StandardOpenOption}
import scala.jdk.CollectionConverters._
import Result._

object Files {

  def findEntriesInDirectory(dir: String, file: String): List[Path] =
    val path = Paths.get(expandPath(file))
    if (JFiles.exists(path) && JFiles.isDirectory(path)) {
      JFiles.list(path).iterator.asScala
        .filter(_.getFileName.toString.startsWith(file))
        .toList.sorted
    } else Nil


  def lcp(names: List[String]): String =
    if (names.isEmpty) ""
    else {
      val first = names.head
      val last = names.last

      first.zip(last).takeWhile(p => p._1 == p._2).map(_._1).mkString
    }

  def listDirectory(dir: java.nio.file.Path): Iterator[Path] = {
    if (JFiles.exists(dir) && JFiles.isDirectory(dir)) {
      JFiles.list(dir).iterator().asScala
    } else {
      Iterator.empty
    }
  }

  def writeToFile(file: String, lines: Iterator[String]): Unit = {
    JFiles.write(
      Paths.get(expandPath(file)), 
      lines.to(Iterable).asJava, 
      StandardOpenOption.CREATE, 
      StandardOpenOption.TRUNCATE_EXISTING)
  }

  def readFromFile(file: String): Result[FileError, Iterator[String]] = {
    val path = Paths.get(expandPath(file))
    if (JFiles.exists(path) && JFiles.isRegularFile(path)) {
      val source = scala.io.Source.fromFile(path.toFile)
      val lines = source.getLines()

      val safeIterator = new Iterator[String] {
        override def hasNext: Boolean = {
          val hasMore = lines.hasNext
          if (!hasMore) source.close()
          hasMore
        }
        override def next(): String = lines.next()
      }
      Success(safeIterator)
      //JFiles.readAllLines(path).asScala.map(_.trim()).filter(_.nonEmpty).toList
    }
    else Fail(FileError.CannotReadFile(s"Cannot read from file: $path"))
  }

  def appendToFile(file: String, lines: Iterator[String]): Unit = {
    JFiles.write(
      Paths.get(expandPath(file)), 
      lines.to(Iterable).asJava, 
      StandardOpenOption.CREATE, 
      StandardOpenOption.APPEND)
  }

  def expandPath(path: String): String = {
    if (path.startsWith("~")) {
      val home = System.getProperty("user.home")
      path.replaceFirst("~", home)
    } else {
      path
    }
  }

}
