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
    editor: EditorState = EditorState(),
    shell: ShellState, 
    cachedSegments: List[Span] = Nil,
    config: SkaldConfig
  ): Unit = {
    
    val currentSegments = if (cachedSegments.isEmpty) PromptEngine.render(shell.current, config) else cachedSegments
    val termWidth = Terminal.getSize().columns

    // --- RENDERING AF DEN AKTIVE LINJE ---
    if (editor.tabCount == 0) {
      if (editor.renderedLines > 0) {
        System.out.print(s"\u001b[${editor.renderedLines}A")
      }
      System.out.print("\r") 
      System.out.print("\u001b[J")

      currentSegments.foreach(s => System.out.print(s"\u001b[0m${if(s.style.bold) "\u001b[1m" else ""}${s.style.foreground}${s.text}"))
      // Her læser vi nu direkte fra state.buffer
      System.out.print(s"\u001b[0m${editor.buffer}")

      val promptLen = currentSegments.map(_.text.length).sum
      val totalChars = promptLen + editor.buffer.length
      val cursorAbsPos = promptLen + editor.cursorIdx
      
      val targetRow = cursorAbsPos / termWidth
      val targetCol = cursorAbsPos % termWidth
      val totalRows = totalChars / termWidth

      val moveUp = totalRows - targetRow
      if (moveUp > 0) System.out.print(s"\u001b[${moveUp}A")
      
      System.out.print("\r")
      if (targetCol > 0) System.out.print(s"\u001b[${targetCol}C")

      System.out.flush()
    }

    val newRows = (currentSegments.map(_.text.length).sum + editor.buffer.length) / termWidth
    val renderedEditor = editor.copy(renderedLines = newRows)

    val input = KeyReader.readKey(Terminal.inputSource)
    input match {

      case CtrlD => ()
      case CtrlC => ()

      case Enter => 
        val cmdLine = editor.buffer.trim
                            // ANSI for rens alt til højre for cursor: bruger til autocompletions
        //System.out.print(s"\r\u001b[K$prompt$buffer")
        System.out.print("\r\n")
        System.out.flush()
        
        if (cmdLine == "undo") {
          shell.history match {
            case previousEnv :: tail =>
              val nextShell = shell.undo
              System.out.print(s"undo: Rolled back shell environment to: ${previousEnv.cwd}\r\n")
              JobManager.reapJobs()
              loop(EditorState(), nextShell, Nil, config)
              
            case Nil =>
              System.out.print("undo: No shell environment to roll back to!\r\n")
              JobManager.reapJobs()
              loop(EditorState(), shell, Nil, config)
          }
        } 
        else if (cmdLine.nonEmpty) {
          val tokens = Lexer.tokenizeInput(cmdLine)
          Parser.parse(tokens, shell.current.aliases) match {
            case Success(command) => {
              HistoryManager.addCommand(cmdLine)
              val (res, nextEnv) = Executor.evaluate(command, shell.current)

              res.output.foreach { item =>
                System.out.print(item.asString + "\n")
                System.out.flush()
              }

              if (res.stderr.nonEmpty) {
                System.out.print(res.stderr)
                System.out.flush()
              }

              JobManager.reapJobs()
              
              loop(EditorState(), shell.update(nextEnv), Nil, config)
            }
            case Fail(err) => 
              System.out.print(err.printError)
              JobManager.reapJobs()
              loop(EditorState(), shell, Nil, config)
          }
        } else {
          JobManager.reapJobs()
          loop(EditorState(), shell, Nil, config)
        }

      case Tab =>
        val currentInput = editor.buffer
        Completer.complete(currentInput, shell.current) match {
          case NoMatch =>
            System.out.print("\u0007")
            loop(editor, shell, currentSegments, config)

          case SingleMatch(text) =>
            val newState = editor.setBuffer(text).copy(tabCount = 0)
            loop(newState, shell, currentSegments, config)

          case MultipleMatches(lcp, options) =>
            if (lcp.length > currentInput.length) {
              val newState = editor.setBuffer(lcp).copy(tabCount = 0)
              loop(newState, shell, currentSegments, config)

            } else if (editor.tabCount == 0) {
              System.out.print("\u0007")
              val newState = editor.copy(tabCount = 1)
              loop(newState, shell, currentSegments, config)
            } else if (editor.tabCount == 1){
              System.out.print("\n" + options.sorted.mkString("  ") + "\n")
              System.out.flush()
              val newState = editor.copy(tabCount = 2)
              loop(newState, shell, currentSegments, config)
            } else {
              System.out.print("\u0007")
              loop(editor, shell, currentSegments, config)
            }
        }

      case Backspace => loop(renderedEditor.backspace, shell, currentSegments, config)        

      case UpArrow => 
        val newHistoryIdx = editor.historyIdx + 1
        if (newHistoryIdx < HistoryManager.size) {
          val out = HistoryManager.getAtIndex(newHistoryIdx)

          val newState = renderedEditor.setBuffer(out).copy(historyIdx = newHistoryIdx)
          loop(newState, shell, currentSegments, config)
        } else {
          loop(editor, shell, currentSegments, config)
        }

      case DownArrow => 
        val newHistoryIdx = editor.historyIdx - 1
        if (newHistoryIdx == -1) {
          val newState = renderedEditor.setBuffer("").copy(historyIdx = -1)
          loop(newState, shell, currentSegments, config)
        } else if (newHistoryIdx >= 0) {
          val out = HistoryManager.getAtIndex(newHistoryIdx)
          val newState = renderedEditor.setBuffer(out).copy(historyIdx = newHistoryIdx)
          loop(newState, shell, currentSegments, config)
        } else {
          loop(renderedEditor, shell, currentSegments, config)
        }

      case LeftArrow =>
        val newState = renderedEditor.copy(cursorIdx = Math.max(0, editor.cursorIdx - 1))
        loop(newState, shell, currentSegments, config)

      case RightArrow =>
        val newState = renderedEditor.copy(cursorIdx = Math.min(editor.buffer.length, editor.cursorIdx + 1))
        loop(newState, shell, currentSegments, config)

      case End => 
        HistoryManager.getSuggestion(editor.buffer) match {
          case Some(suggestion) =>
            val newState = renderedEditor.setBuffer(suggestion)
            loop(newState, shell, currentSegments, config)
          case None => 
            loop(renderedEditor, shell, currentSegments, config)
        }

      case CharKey(c) => 
        loop(renderedEditor.insertChar(c), shell, currentSegments, config)

      case Escape => 
        val newState = renderedEditor.copy(tabCount = 0)
        loop(newState, shell, currentSegments, config)

      case Unknown =>
        System.out.print("\u0007")
        loop(renderedEditor, shell, currentSegments, config)
    }
  }

  try {
    Terminal.setRaw()
    HistoryManager.init()
    
    val baseEnv = ShellEnv()
    val startingEnv = ConfigLoader.loadRc(baseEnv)
    val config = ConfigLoader.loadConfig

    val initialShell = ShellState(current = startingEnv)

    loop(shell = initialShell, config = config)
  } finally {
    Terminal.restore()
    HistoryManager.save()

    // ANSI kode for at ændre den tilbage til blok cursor
    System.out.print("\u001b[2 q")
    System.out.flush()
  }
}
