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

  def isPath(filePath: String): Boolean = {
    val absoluteTarget = Paths.get(filePath).toAbsolutePath.normalize()
    
    // Hent PATH mapperne som en mængde (Set) af absolutte, normaliserede stier
    val pathDirs = sys.env.getOrElse("PATH", "")
      .split(pathSep)
      .filter(_.nonEmpty)
      .map(p => Paths.get(p).toAbsolutePath.normalize())
      .toSet

    // Tjek om filens parent directory er en del af PATH, og om filen er eksekverbar
    Option(absoluteTarget.getParent)
      .exists(parent => pathDirs.contains(parent) && Files.isExecutable(absoluteTarget))
  }
}
