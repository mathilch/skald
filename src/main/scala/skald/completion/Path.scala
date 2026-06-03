package skald

import java.nio.file.{Files, Paths, Path}
import scala.jdk.CollectionConverters._
import scala.util.Using

object Path {
  private val pathSep = java.io.File.pathSeparator

  def getAllExecutables(): List[String] = {
    val pathEnv = sys.env.getOrElse("PATH", "")
    
    pathEnv.split(pathSep)
      .filter(_.nonEmpty)
      .map(Paths.get(_))
      .filter(Files.isDirectory(_))
      .flatMap { dir =>
        Using(Files.newDirectoryStream(dir)) { stream =>
          stream.asScala
            .filter(p => Files.isRegularFile(p) && Files.isExecutable(p))
            .map(_.getFileName.toString)
            .toList
        }.getOrElse(Nil) 
      }
      .toList
      .distinct
  }

  def findInPath(name: String): Option[String] = {
    val pathEnv = sys.env.getOrElse("PATH", "")
    
    pathEnv.split(pathSep)
      .filter(_.nonEmpty)
      .map(Paths.get(_, name))
      .find(p => Files.isRegularFile(p) && Files.isExecutable(p))
      .map(_.toAbsolutePath.toString)
  }
}
