package skald

import java.nio.file.Path

case class ExecutionResult(
  output: Iterator[ShellData] = Iterator.empty,
  stderr: String = "",
  exitCode: Int = 0
) {
  def stdout: String =
    if (output.isEmpty) ""
    else output.map(_.asString).mkString("\n") + "\n"
}


enum ShellData:
  case Text(str: String)
  case FileNode(path: Path)
  case ProcessInfo(pid: Int, name: String)

  def asString: String = this match {
    case Text(str) => str
    case FileNode(path) => path.getFileName.toString
    case ProcessInfo(pid, name) => s"[$pid] $name"
  }
