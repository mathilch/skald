package skald 

import scala.io.StdIn
import scala.sys.process._
import scala.annotation.tailrec
import Completion._
import Key._
import Result._

object Main extends App {
  Runtime.getRuntime.addShutdownHook(new Thread(() => {Terminal.restore()}))
 
  @tailrec
  def loop(
    buffer: StringBuilder, 
    state: EditorState = EditorState(),
    env: ShellEnv = ShellEnv(),
    envHistory: List[ShellEnv] = Nil,
    cachedPrompt: String = "",
    config: SkaldConfig
  ): Unit = {
    
    val prompt = 
      if (cachedPrompt.isEmpty()) PromptEngine.render(env, config)
      else cachedPrompt

    if (state.tabCount == 0) {
      val currentInput = buffer.toString
      val suggestionOpt = HistoryManager.getSuggestion(currentInput)

      // 1. Ryd linjen, print prompt og det faktiske input
      System.out.print(s"\r\u001b[J$prompt$buffer")

      // 2. Hvis vi har et forslag, og cursoren er for enden af inputtet, så tegn rest-delen
      if (suggestionOpt.isDefined && state.cursorIdx == currentInput.length) {
        val suggestion = suggestionOpt.get
        val remainder = suggestion.substring(currentInput.length)
        
        // \u001b[90m = Lys grå (eller \u001b[38;5;8m). \u001b[0m = reset
        System.out.print(s"\u001b[90m$remainder\u001b[0m")
      }

      // 3. Beregn, hvor cursoren FAKTISK skal være, og ryk den dertil.
      val visiblePromptLen = prompt.replaceAll("\u001b\\[[0-9;]*[a-zA-Z]", "").length
      val cursorCol = visiblePromptLen + state.cursorIdx + 1
      System.out.print(s"\u001b[${cursorCol}G")
      System.out.flush()
    }

    val input = KeyReader.readKey(Terminal.inputSource)
    input match {

      case CtrlD => ()
      case CtrlC => ()

      case Enter => 
        val cmdLine = buffer.toString.trim
                            // ANSI for rens alt til højre for cursor: bruger til autocompletions
        System.out.print(s"\r\u001b[K$prompt$buffer")
        System.out.print("\r\n")
        System.out.flush()
        
        if (cmdLine == "undo") {
          envHistory match {
            case previousEnv :: tail =>
              System.out.print(s"undo: Rolled back shell environment to: ${previousEnv.cwd}\r\n")
              JobManager.reapJobs()
              loop(new StringBuilder(), EditorState(), previousEnv, envHistory = tail, cachedPrompt = "", config = config)
              
            case Nil =>
              System.out.print("undo: No shell environment to roll back to!\r\n")
              JobManager.reapJobs()
              loop(new StringBuilder(), EditorState(), env, envHistory = Nil, cachedPrompt = "", config = config)
          }
        } 
        else if (cmdLine.nonEmpty) {
          val tokens = Lexer.tokenizeInput(cmdLine)
          Parser.parse(tokens, env.aliases) match {
            case Success(command) => {
              HistoryManager.addCommand(cmdLine)
              val (res, nextEnv) = Executor.evaluate(command, env)

              res.output.foreach { item =>
                System.out.print(item.asString + "\n")
                System.out.flush()
              }

              if (res.stderr.nonEmpty) {
                System.out.print(res.stderr)
                System.out.flush()
              }

              JobManager.reapJobs()
              
              val updatedHistory = if (nextEnv != env) env :: envHistory else envHistory
              loop(new StringBuilder(), EditorState(), nextEnv, updatedHistory, cachedPrompt = "", config = config)
            }
            case Fail(err) => 
              System.out.print(err.printError)
              JobManager.reapJobs()
              loop(new StringBuilder(), EditorState(), env, envHistory, cachedPrompt = "", config = config)
          }
        } else {
          JobManager.reapJobs()
          loop(new StringBuilder(), EditorState(), env, envHistory, cachedPrompt = "", config = config)
        }

      case Tab =>
        val currentInput = buffer.toString()
        Completer.complete(currentInput, env) match {
          case NoMatch =>
            System.out.print("\u0007")
            loop(buffer, state, env, envHistory, cachedPrompt = prompt, config = config)

          case SingleMatch(text) =>
            buffer.clear()
            buffer.append(text)
            //System.out.print(s"\r\u001b[K$prompt$buffer")
            val newState = state.copy(tabCount = 0, cursorIdx = text.length)
            loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)

          case MultipleMatches(lcp, options) =>
            if (lcp.length > currentInput.length) {
              buffer.clear()
              buffer.append(lcp)
              //System.out.print(s"\r\u001b[K$prompt$buffer")

              val newState = state.copy(tabCount = 0, cursorIdx = lcp.length)
              loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)

            } else if (state.tabCount == 0) {
              System.out.print("\u0007")
              val newState = state.copy(tabCount = 1)
              loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)
            } else if (state.tabCount == 1){
              System.out.print("\n" + options.sorted.mkString("  ") + "\n")
              System.out.print(s"$prompt$buffer")
              System.out.flush()
              val newState = state.copy(tabCount = 2)
              loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)
            } else {
              System.out.print("\u0007")
              loop(buffer, state, env, envHistory, cachedPrompt = prompt, config = config)
            }
        }

      case Backspace => 
        if (buffer.nonEmpty && state.cursorIdx > 0) {
          buffer.deleteCharAt(state.cursorIdx - 1)
          val newState = state.copy(cursorIdx = state.cursorIdx - 1)
          loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)
        } else {
          loop(buffer, state, env, envHistory, cachedPrompt = prompt, config = config)
        }

      case UpArrow => 
        val newHistoryIdx = state.historyIdx + 1
        if (newHistoryIdx < HistoryManager.size) {
          val out = HistoryManager.getAtIndex(newHistoryIdx)

          buffer.clear()
          buffer.append(out)

          val newState = state.copy(historyIdx = newHistoryIdx, cursorIdx = out.length)
          loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)
        } else {
          loop(buffer, state, env, envHistory, cachedPrompt = prompt, config = config)
        }

      case DownArrow => 
        val newHistoryIdx = state.historyIdx - 1
        val out = HistoryManager.getAtIndex(newHistoryIdx)

        buffer.clear()
        buffer.append(out)

        if (newHistoryIdx == -1) {
          val newState = state.copy(cursorIdx = 0)
          loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)
        } else {
          val newState = state.copy(historyIdx = newHistoryIdx, cursorIdx = out.length)
          loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)
        }

      case LeftArrow =>
        val newState = state.copy(cursorIdx = Math.max(0, state.cursorIdx - 1))
        loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)

      case RightArrow =>
        val newState = state.copy(cursorIdx = Math.min(buffer.length, state.cursorIdx + 1))
        loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)

      case End => 
        HistoryManager.getSuggestion(buffer.toString) match {
          case Some(suggestion) =>
            buffer.clear()
            buffer.append(suggestion)
            val newState = state.copy(cursorIdx = suggestion.length)
            loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)
          case None => 
            loop(buffer, state, env, envHistory, cachedPrompt = prompt, config = config)
        }

      case CharKey(c) => 
        buffer.insert(state.cursorIdx, c)
        val newState = state.copy(cursorIdx = state.cursorIdx + 1, historyIdx = -1, tabCount = 0)
        loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)

      case Escape => 
        val newState = state.copy(tabCount = 0)
        loop(buffer, newState, env, envHistory, cachedPrompt = prompt, config = config)

      case Unknown =>
        System.out.print("\u0007")
        loop(buffer, state, env, envHistory, cachedPrompt = prompt, config = config)
    }
  }

  try {
    Terminal.setRaw()
    HistoryManager.init()
    
    val baseEnv = ShellEnv()
    val startingEnv = ConfigLoader.loadRc(baseEnv)
    val config = ConfigLoader.loadConfig

    loop(new StringBuilder(), env = startingEnv, config = config)
  } finally {
    Terminal.restore()
    HistoryManager.save()

    // ANSI kode for at ændre den tilbage til blok cursor
    System.out.print("\u001b[2 q")
    System.out.flush()
  }
}
