package skald 

import skald.terminal.grid.Span

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
    state: EditorState = EditorState(),
    env: ShellEnv = ShellEnv(),
    envHistory: List[ShellEnv] = Nil,
    cachedSegments: List[Span] = Nil,
    config: SkaldConfig
  ): Unit = {
    
    val currentSegments = if (cachedSegments.isEmpty) PromptEngine.render(env, config) else cachedSegments
    val termWidth = Terminal.getSize().columns

    // --- RENDERING AF DEN AKTIVE LINJE ---
    if (state.tabCount == 0) {
      if (state.renderedLines > 0) {
        System.out.print(s"\u001b[${state.renderedLines}A")
      }
      System.out.print("\r") 
      System.out.print("\u001b[J")

      currentSegments.foreach(s => System.out.print(s"\u001b[0m${if(s.style.bold) "\u001b[1m" else ""}${s.style.foreground}${s.text}"))
      // Her læser vi nu direkte fra state.buffer
      System.out.print(s"\u001b[0m${state.buffer}")

      val promptLen = currentSegments.map(_.text.length).sum
      val totalChars = promptLen + state.buffer.length
      val cursorAbsPos = promptLen + state.cursorIdx
      
      val targetRow = cursorAbsPos / termWidth
      val targetCol = cursorAbsPos % termWidth
      val totalRows = totalChars / termWidth

      val moveUp = totalRows - targetRow
      if (moveUp > 0) System.out.print(s"\u001b[${moveUp}A")
      
      System.out.print("\r")
      if (targetCol > 0) System.out.print(s"\u001b[${targetCol}C")

      System.out.flush()
    }

    val newRows = (currentSegments.map(_.text.length).sum + state.buffer.length) / termWidth
    val renderState = state.copy(renderedLines = newRows)

    val input = KeyReader.readKey(Terminal.inputSource)
    input match {

      case CtrlD => ()
      case CtrlC => ()

      case Enter => 
        val cmdLine = state.buffer.trim
                            // ANSI for rens alt til højre for cursor: bruger til autocompletions
        //System.out.print(s"\r\u001b[K$prompt$buffer")
        System.out.print("\r\n")
        System.out.flush()
        
        if (cmdLine == "undo") {
          envHistory match {
            case previousEnv :: tail =>
              System.out.print(s"undo: Rolled back shell environment to: ${previousEnv.cwd}\r\n")
              JobManager.reapJobs()
              loop(EditorState(), previousEnv, envHistory = tail, cachedSegments = Nil, config = config)
              
            case Nil =>
              System.out.print("undo: No shell environment to roll back to!\r\n")
              JobManager.reapJobs()
              loop(EditorState(), env, envHistory = Nil, cachedSegments = Nil, config = config)
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
              loop(EditorState(), nextEnv, updatedHistory, cachedSegments = Nil, config = config)
            }
            case Fail(err) => 
              System.out.print(err.printError)
              JobManager.reapJobs()
              loop(EditorState(), env, envHistory, cachedSegments = Nil, config = config)
          }
        } else {
          JobManager.reapJobs()
          loop(EditorState(), env, envHistory, cachedSegments = Nil, config = config)
        }

      case Tab =>
        val currentInput = state.buffer
        Completer.complete(currentInput, env) match {
          case NoMatch =>
            System.out.print("\u0007")
            loop(state, env, envHistory, cachedSegments = currentSegments, config = config)

          case SingleMatch(text) =>
            val newState = state.setBuffer(text).copy(tabCount = 0)
            loop(newState, env, envHistory, cachedSegments = currentSegments, config = config)

          case MultipleMatches(lcp, options) =>
            if (lcp.length > currentInput.length) {
              val newState = state.setBuffer(lcp).copy(tabCount = 0)
              loop(newState, env, envHistory, cachedSegments = currentSegments, config = config)

            } else if (state.tabCount == 0) {
              System.out.print("\u0007")
              val newState = state.copy(tabCount = 1)
              loop(newState, env, envHistory, cachedSegments = currentSegments, config = config)
            } else if (state.tabCount == 1){
              System.out.print("\n" + options.sorted.mkString("  ") + "\n")
              System.out.flush()
              val newState = state.copy(tabCount = 2)
              loop(newState, env, envHistory, cachedSegments = currentSegments, config = config)
            } else {
              System.out.print("\u0007")
              loop(state, env, envHistory, cachedSegments = currentSegments, config = config)
            }
        }

      case Backspace => loop(renderState.backspace, env, envHistory, currentSegments, config)        

      case UpArrow => 
        val newHistoryIdx = state.historyIdx + 1
        if (newHistoryIdx < HistoryManager.size) {
          val out = HistoryManager.getAtIndex(newHistoryIdx)

          val newState = renderState.setBuffer(out).copy(historyIdx = newHistoryIdx)
          loop(newState, env, envHistory, cachedSegments = currentSegments, config = config)
        } else {
          loop(state, env, envHistory, cachedSegments = currentSegments, config = config)
        }

      case DownArrow => 
        val newHistoryIdx = state.historyIdx - 1
        if (newHistoryIdx == -1) {
          val newState = renderState.setBuffer("").copy(historyIdx = -1)
          loop(newState, env, envHistory, currentSegments, config)
        } else if (newHistoryIdx >= 0) {
          val out = HistoryManager.getAtIndex(newHistoryIdx)
          val newState = renderState.setBuffer(out).copy(historyIdx = newHistoryIdx)
          loop(newState, env, envHistory, currentSegments, config)
        } else {
          loop(renderState, env, envHistory, currentSegments, config)
        }

      case LeftArrow =>
        val newState = renderState.copy(cursorIdx = Math.max(0, state.cursorIdx - 1))
        loop(newState, env, envHistory, cachedSegments = currentSegments, config = config)

      case RightArrow =>
        val newState = renderState.copy(cursorIdx = Math.min(state.buffer.length, state.cursorIdx + 1))
        loop(newState, env, envHistory, cachedSegments = currentSegments, config = config)

      case End => 
        HistoryManager.getSuggestion(state.buffer) match {
          case Some(suggestion) =>
            val newState = renderState.setBuffer(suggestion)
            loop(newState, env, envHistory, cachedSegments = currentSegments, config = config)
          case None => 
            loop(renderState, env, envHistory, cachedSegments = currentSegments, config = config)
        }

      case CharKey(c) => 
        loop(renderState.insertChar(c), env, envHistory, cachedSegments = currentSegments, config = config)

      case Escape => 
        val newState = renderState.copy(tabCount = 0)
        loop(newState, env, envHistory, cachedSegments = currentSegments, config = config)

      case Unknown =>
        System.out.print("\u0007")
        loop(renderState, env, envHistory, cachedSegments = currentSegments, config = config)
    }
  }

  try {
    Terminal.setRaw()
    HistoryManager.init()
    
    val baseEnv = ShellEnv()
    val startingEnv = ConfigLoader.loadRc(baseEnv)
    val config = ConfigLoader.loadConfig

    loop(env = startingEnv, config = config)
  } finally {
    Terminal.restore()
    HistoryManager.save()

    // ANSI kode for at ændre den tilbage til blok cursor
    System.out.print("\u001b[2 q")
    System.out.flush()
  }
}
