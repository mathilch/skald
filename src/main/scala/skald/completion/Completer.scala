package skald

import Completion._
import java.nio.file.{Files => JFiles}

object Completer {
  def complete(currentInput: String, env: ShellEnv): Completion = {
    val tokens = Lexer.tokenizeInput(currentInput)
    val isTrailingSpace = currentInput.endsWith(" ")

    if tokens.isEmpty then completeCommand("")
    else if (tokens.size == 1 && !isTrailingSpace) then completeCommand(tokens.head)
    else {
      val cmd = tokens.head 
      val arg = if (isTrailingSpace) "" else tokens.last

      if (isTrailingSpace && tokens.size > 1) {
        NoMatch
      } else {
        val execPrefix = if (isTrailingSpace) {
          currentInput
        } else {
          val lastArgIndex = currentInput.lastIndexOf(arg)
          if (lastArgIndex != -1) currentInput.substring(0, lastArgIndex)
          else currentInput.substring(0, currentInput.lastIndexOf(' ') + 1)
        }
        completeArgument(cmd, arg, execPrefix, currentInput, env)
      }
    }
  }

  private def completeCommand(input: String) = {
    val trie = Trie.load()
    val matches = trie.findPrefix(input)
      .map(_.complete(input))
      .getOrElse(Nil)
  
    matches match {
      case Nil => NoMatch
      case x :: Nil => 
        SingleMatch(x + " ")
      case multiple =>
        val lcp = trie.lcpForAll(multiple)
        MultipleMatches(lcp, multiple)
    }
  }

  private def completeArgument(cmd: String, arg: String, execPrefix: String, buffer: String, env: ShellEnv) = {
    CompletionRegistry.get(cmd.trim()) match {       // /tmp/pig/singleCompleter docker ' ' 
      case Some(scriptPath) => completeScriptFromRegistry(scriptPath, cmd, arg, execPrefix, buffer,env)
      case None => completeFileSystem(execPrefix, arg, env)
    }
  }

  private def completeFileSystem(execPrefix: String, arg: String, env: ShellEnv) = {

    val lastSlash = arg.lastIndexOf("/")
    
    val searchPath = if (lastSlash == -1) {
      env.cwd
    } else {
      env.cwd.resolve(arg.substring(0, lastSlash + 1))
    }
    
    val searchPattern = arg.substring(lastSlash + 1)

    // Convert searchPath back to String for your Files.findEntriesInDirectory method
    val matches = Files.findEntriesInDirectory(searchPath.toString, searchPattern)


    matches match {
      case Nil => NoMatch
      
      case x :: Nil =>
        val suffix = if (JFiles.isDirectory(x)) "/" else " "
        val completedPath = 
          if (lastSlash == -1) x.getFileName.toString
          else arg.substring(0, lastSlash + 1) + x.getFileName.toString
        SingleMatch(s"$execPrefix$completedPath$suffix")
      
      case multiples =>
        val formatted = multiples.map { f =>
          val name = f.getFileName.toString
          if (JFiles.isDirectory(f)) s"$name/" else s"$name "
        }

        val lcp = Files.lcp(formatted)
        val prefixPath = if (lastSlash == -1) "" else arg.substring(0, lastSlash + 1)
        val fullLcp = s"$execPrefix$prefixPath$lcp"

        MultipleMatches(fullLcp, formatted)
    }
  }

  private def completeScriptFromRegistry(scriptPath: String, cmd: String, arg: String, prev: String, buffer: String, env: ShellEnv) = {
    import scala.sys.process._
    try {
      val envVars = Seq(
        "COMP_LINE" -> buffer,
        "COMP_POINT" -> buffer.length.toString
      )
      val output = Process(Seq(scriptPath, cmd, arg, prev), env.cwd.toFile(), envVars*).lazyLines.toList
      output match {
        case Nil => NoMatch 
        case x :: Nil => 
          // Safely append to the buffer up to the start of the argument
          val res = s"${buffer.substring(0, buffer.lastIndexOf(arg))}$x "
          SingleMatch(res)
        case multiple => 
          val lcp = Files.lcp(multiple)
          val res = s"${buffer.substring(0, buffer.lastIndexOf(arg))}$lcp"
          MultipleMatches(res, multiple)
      }
    } catch {
      case _: Exception => NoMatch
    }
  }

}
