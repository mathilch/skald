package skald 

import scala.io.StdIn
import scala.sys.process._
import scala.annotation.tailrec

object Main extends App {
  Runtime.getRuntime.addShutdownHook(new Thread(() => {RawTerminal.restore()}))
 
  @tailrec
  def loop(
    buffer: StringBuilder, 
    tabCount: Int = 0, 
    historyIdx: Int = -1, 
    env: ShellEnv = ShellEnv(),
    cachedPrompt: String = ""
  ): Unit = {
    
    val prompt = 
      if (cachedPrompt.isEmpty()) PromptEngine.render(env)
      else cachedPrompt

    if (tabCount == 0) {
      System.out.print(s"\r\u001b[K$prompt$buffer")
      System.out.flush()
    }

    val input = KeyReader.readKey(RawTerminal.inputSource)
    input match {

      case CtrlD => ()
      case CtrlC => ()

      case Enter => 
        val cmdLine = buffer.toString.trim
        System.out.print("\r\n")

        if (cmdLine.nonEmpty) {
          val tokens = Lexer.tokenizeInput(cmdLine)
          Parser.parse(tokens) match {
            case Some(command) => {
              HistoryManager.addCommand(cmdLine)
              val (res, nextEnv) = Executor.evaluate(command, env)
              JobManager.reapJobs()
              loop(new StringBuilder(), 0, -1, nextEnv, cachedPrompt = "")
            }
            case None => 
              System.out.print(s"Unknown\r\n")
              JobManager.reapJobs()
              loop(new StringBuilder(), 0, -1, env, cachedPrompt = "")
          }
        } else {
          JobManager.reapJobs()
          loop(new StringBuilder(), 0, -1, env, cachedPrompt = "")
        }

      case Tab =>
        val currentInput = buffer.toString()
        Completer.complete(currentInput, env) match {
          case NoMatch =>
            System.out.print("\u0007")
            loop(buffer, 0, historyIdx, env, cachedPrompt = prompt)

          case SingleMatch(text) =>
            buffer.clear()
            buffer.append(text)
            System.out.print(s"\r\u001b[K$prompt$buffer")
            loop(buffer, 0, historyIdx, env, cachedPrompt = prompt)

          case MultipleMatches(lcp, options) =>
            if (lcp.length > currentInput.length) {
              buffer.clear()
              buffer.append(lcp)
              System.out.print(s"\r\u001b[K$prompt$buffer")
              loop(buffer, 0, historyIdx, env, cachedPrompt = prompt)
            } else if (tabCount == 0) {
              System.out.print("\u0007")
              loop(buffer, 1, historyIdx, env, cachedPrompt = prompt)
            } else if (tabCount == 1){
              System.out.print("\n" + options.sorted.mkString("  ") + "\n")
              System.out.print(s"$prompt$buffer")
              System.out.flush()
              loop(buffer, 2, historyIdx, env, cachedPrompt = prompt)
            } else {
              System.out.print("\u0007")
              loop(buffer, 2, historyIdx, env, cachedPrompt = prompt)
            }
        }

      case Backspace => 
        if (buffer.nonEmpty) buffer.deleteCharAt(buffer.length - 1)
        loop(buffer, 0, historyIdx, env, cachedPrompt = prompt)

      case UpArrow => 
        val idx = historyIdx + 1

        if (idx < HistoryManager.size) {
          val out = HistoryManager.getAtIndex(idx)

          buffer.clear()
          buffer.append(out)

          loop(buffer, 0, idx, env, cachedPrompt = prompt)
        } else {
          loop(buffer, 0, historyIdx, env, cachedPrompt = prompt)
        }

      case DownArrow => 
        val idx = historyIdx - 1
        val out = HistoryManager.getAtIndex(idx)


        buffer.clear()
        buffer.append(out)

        if (historyIdx == -1) {
          loop(buffer, 0, historyIdx, env, cachedPrompt = prompt)
        } else
        loop(buffer, 0, idx, env, cachedPrompt = prompt)

      case CharKey(c) => 
        buffer.append(c)
        loop(buffer, 0, -1, env, cachedPrompt = prompt)

      case Escape => loop(buffer, 0, -1, env, cachedPrompt = prompt)

      case Unknown =>
        System.out.print("\u0007")
        loop(buffer, 0, -1, env, cachedPrompt = prompt)
    }
  }

  try {
    RawTerminal.setRaw()
    HistoryManager.init()
    loop(new StringBuilder())
  } finally {
    RawTerminal.restore()
    HistoryManager.save()
  }
}
