package skald

import scala.sys.process._
import java.nio.file.Path

enum GitStatus:
  case NotARepository
  case Branch(name: String)

object GitStatus:
  def fromPath(cwd: Path): GitStatus = {
    try {
      val noErrLogger = ProcessLogger(out => (), err => ())
      val process = Process(Seq("git", "branch", "--show-current"), cwd.toFile)
      process.lazyLines(noErrLogger).headOption match {
        case Some(name) if name.nonEmpty => Branch(name)
        case _ => NotARepository
      }
    } catch {
      case _: Exception => NotARepository
    }
  }
