package skald

import Result._

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.util.{Using, Success => TrySuccess, Failure => TryFailure}

object ConfigLoader {

  def loadConfig: SkaldConfig = {
    val path = Paths.get(System.getProperty("user.home"), ".config", "skald", "skald.conf")
    val defaultConfig = SkaldConfig()

    if (!Files.exists(path)) {
      return defaultConfig
    }

    Using(Source.fromFile(path.toFile)) { source =>
      source.getLines().foldLeft(defaultConfig) { (config, line) =>
        val trimmed = line.trim
        if (trimmed.isEmpty || trimmed.startsWith("#")) {
          config
        } else {
          trimmed.split("=", 2) match {
            case Array("prompt", value) => 
              config.copy(promptTemplate = value)
            case Array("dirDepth", value) => 
              config.copy(dirDepth = value.toIntOption.filter(_ > 0))
            case Array("gitFormat", value) =>
              config.copy(gitFormat = value)
            case Array("cursor", value) =>
              config.copy(cursorStyle = CursorStyle.fromString(value).getOrElse(config.cursorStyle))
            case _ => config
          }
        }
      }
    } match {
      case TrySuccess(conf) => conf
      case TryFailure(exception) => 
        System.err.println(s"[ERROR]: Could not read skald.conf: $exception")
        defaultConfig
    }
  }


  def loadRc(initialEnv: ShellEnv): ShellEnv = {
    val rcPath = Paths.get(System.getProperty("user.home"), ".skaldrc")
    if (Files.exists(rcPath)) loadFromFile(rcPath, initialEnv)
    else initialEnv
  }

  def loadFromFile(path: Path, env: ShellEnv): ShellEnv = {
    if (!Files.exists(path)) {
      System.err.println(s"skald: source: ${path.toString}: No such file")
      return env
    }

    Using(Source.fromFile(path.toFile)) { source =>
      source.getLines().foldLeft(env) { (currentEnv, line) =>
        val trimmed = line.trim
        if (trimmed.isEmpty || trimmed.startsWith("#")) {
          currentEnv
        } else {
          executeLine(trimmed, currentEnv)
        }
      }
    } match {
      case TrySuccess(finalEnv) => finalEnv
      case TryFailure(err) =>
        System.err.println(s"[ERROR] reading ${path.toString}: ${err.getMessage}")
        env
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
