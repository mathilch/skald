package skald

import Result._

import java.io.File
import scala.io.Source

object ConfigLoader {
  def loadRc(initialEnv: ShellEnv): ShellEnv = {
    val rcFile = new File(System.getProperty("user.home"), ".skaldrc")

    if (!rcFile.exists()) return initialEnv

    val source = Source.fromFile(rcFile)
    try {
      source.getLines().foldLeft(initialEnv) { (currentEnv, line) =>
        val trimmed = line.trim
        if (trimmed.isEmpty || trimmed.startsWith("#")) {
          currentEnv
        } else {
          executeLine(trimmed, currentEnv)
        }
      }
    } finally {
      source.close()
    }
  }

  private def executeLine(line: String, env: ShellEnv): ShellEnv = {
    val tokens = Lexer.tokenizeInput(line)
    Parser.parse(tokens, env.aliases) match {
      case Success(cmd) =>
        val (_, nextEnv) = Executor.evaluate(cmd, env)
        nextEnv
      case Fail(err) =>
        System.err.println(s".skaldrc error: $err on line: $line")
        env
    }
  }
}
