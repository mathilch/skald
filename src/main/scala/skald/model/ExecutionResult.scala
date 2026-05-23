package skald

import java.nio.file.Path
import java.nio.file.{Files => JFiles}

case class ExecutionResult(
  output: Iterator[ShellData] = Iterator.empty,
  stderr: String = "",
  exitCode: Int = 0
) {
  def stdout: String =
    val items = output.toList
    if (items.isEmpty) ""
    else items.map(_.asString).mkString("\n") + "\n"
}


