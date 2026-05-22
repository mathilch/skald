package skald

import Completion._

object Completer {
  def complete(currentInput: String, env: ShellEnv): Completion = {
    val lastSpace = currentInput.lastIndexOf(' ')

    if (lastSpace == -1) {
      completeCommand(currentInput)
    } else {
      val parts = currentInput.split(" ", -1)
      val argv1 = parts.head
      val argv2 = parts.last
      val argv3 = if (parts.length >= 2) parts(parts.length - 2) else ""

      // val cmd = currentInput.substring(0, lastSpace + 1)
      // val arg = currentInput.substring(lastSpace + 1)
      completeArgument(argv1, argv2, argv3, currentInput, env)
    }
  }

  private def completeCommand(input: String) = {
    val trie = Trie.load()
    val matches = trie.findPrefix(input)
      .map(_.complete(input))
      .getOrElse(Nil)

    matches match {
      case Nil => NoMatch
      case x :: Nil => SingleMatch(x + " ")
      case multiple =>
        val lcp = trie.lcpForAll(multiple)
        MultipleMatches(lcp, multiple)
    }
  }

  private def completeArgument(cmd: String, arg: String, prev: String, buffer: String, env: ShellEnv) = {
    CompletionRegistry.get(cmd.trim()) match {       // /tmp/pig/singleCompleter docker ' ' 
      case Some(scriptPath) => completeScriptFromRegistry(scriptPath, cmd, arg, prev, buffer,env)
      case None =>
        val res = if (cmd == prev) s"$cmd " else s"$cmd $prev "
        completeFileSystem(res, arg, env)
    }
  }

  private def completeFileSystem(exec: String, fullArg: String, env: ShellEnv) = {
    val lastSlash = fullArg.lastIndexOf("/")
    val path = if (lastSlash == -1) env.cwd.toString() else fullArg.substring(0, lastSlash + 1)
    val searchPattern = fullArg.substring(lastSlash + 1)

    val matches = Files.findEntriesInDirectory(path, searchPattern)
    matches match {
      case Nil => NoMatch
      
      case x :: Nil =>
        val suffix = if (x.isDirectory()) "/" else " "
        val completedPath = 
          if (lastSlash == -1) x.getName 
          else fullArg.substring(0, lastSlash + 1) + x.getName
        SingleMatch(s"$exec$completedPath$suffix")
      
      case multiples =>
        val formatted = multiples.map { f =>
          val name = f.getName()
          if (f.isDirectory()) s"$name/" else s"$name "
        }

        val lcp = Files.lcp(formatted)
        val prefixPath = if (lastSlash == -1) "" else fullArg.substring(0, lastSlash + 1)
        val fullLcp = s"$exec$prefixPath$lcp"

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
