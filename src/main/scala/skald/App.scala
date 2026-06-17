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
    prompt: List[Span] = Nil,
    config: SkaldConfig,
    screenOutput: List[List[Span]] = Nil
  ): Unit = {
    
    val activePrompt = if (prompt.isEmpty) PromptEngine.render(shell.current, config) else prompt
    val termWidth = Terminal.getSize().columns
    val newRenderedLine = TerminalRenderer.render(editor, activePrompt, termWidth)
    val renderedEditor = editor.copy(renderedLines = newRenderedLine)

    val input = KeyReader.readKey(Terminal.inputSource)
    input match {

      case CtrlD => 
        if (editor.buffer.isEmpty) System.out.print("\r\nBye!\r\n") // Exit shell hvis linjen er tom
        else loop(renderedEditor, shell, activePrompt, config)

      case CtrlC => 
        System.out.print("^C\r\n")
        loop(EditorState(), shell, Nil, config) 

      case Enter =>
        val cmdLine = editor.buffer.trim
          System.out.print("\r\n")
          System.out.flush()
          
          if (cmdLine == "undo") {
            val nextShell = shell.undo
            if (nextShell != shell) {
              System.out.print(s"undo: Rolled back shell environment to: ${nextShell.current.cwd}\r\n")
            } else {
              System.out.print("undo: No shell environment to roll back to!\r\n")
            }
            JobManager.reapJobs()
            loop(EditorState(), nextShell, Nil, config)
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
        editor.tabState match {
          case TabState.Inactive =>
            val currentInput = editor.buffer
            Completer.complete(currentInput, shell.current) match {
              case NoMatch => 
                System.out.print("\u0007")
                loop(editor, shell, activePrompt, config)

              case SingleMatch(completed) => 
                loop(editor.setBuffer(completed), shell, activePrompt, config)

              case MultipleMatches(lcp, options) => 
                if (lcp.length > renderedEditor.buffer.length) {
                  loop(renderedEditor.setBuffer(lcp), shell, activePrompt, config)
                } else {
                  val nextState = renderedEditor.copy(
                    tabState = TabState.Active(editor.buffer, options.sorted.toVector, 0)
                  )
                  System.out.println("\n" + options.sorted.mkString(" "))
                  loop(nextState, shell, activePrompt, config)
                }
            }
          case TabState.Active(prefix, options, idx) =>
            val currentSelection = options(idx)
            val newBuffer = prefix + currentSelection

            val nextIdx = (idx + 1) % options.length
            val nextState = renderedEditor.updateBuffer(newBuffer).copy(
              tabState = TabState.Active(prefix, options, nextIdx)
            )

            loop(nextState, shell, activePrompt, config)
        }


        // val currentInput = renderedEditor.buffer
        // Completer.complete(currentInput, shell.current) match {
        //   case NoMatch =>
        //     System.out.print("\u0007")
        //     loop(renderedEditor, shell, activePrompt, config)
        //
        //   case SingleMatch(text) =>
        //     val newState = renderedEditor.setBuffer(text).copy(tabCount = 0)
        //     loop(newState, shell, activePrompt, config)
        //
        //   case MultipleMatches(lcp, options) =>
        //     if (lcp.length > currentInput.length) {
        //       val newState = renderedEditor.setBuffer(lcp).copy(tabCount = 0)
        //       loop(newState, shell, activePrompt, config)
        //
        //     } else if (editor.tabCount == 0) {
        //       System.out.print("\u0007")
        //       val newState = renderedEditor.copy(tabCount = 1)
        //       loop(newState, shell, activePrompt, config)
        //     } else if (editor.tabCount == 1){
        //       System.out.print("\n" + options.sorted.mkString("  ") + "\n")
        //       System.out.flush()
        //       val newState = renderedEditor.copy(tabCount = 2)
        //       loop(newState, shell, activePrompt, config)
        //     } else {
        //       System.out.print("\u0007")
        //       loop(editor, shell, activePrompt, config)
        //     }
        // }

      case Backspace => loop(renderedEditor.backspace, shell, activePrompt, config)

      case UpArrow => 
        val newHistoryIdx = renderedEditor.historyIdx + 1
        if (newHistoryIdx < HistoryManager.size) {
          val out = HistoryManager.getAtIndex(newHistoryIdx)

          val newState = renderedEditor.setBuffer(out).copy(historyIdx = newHistoryIdx)
          loop(newState, shell, activePrompt, config)
        } else {
          loop(renderedEditor.copy(tabState = TabState.Inactive), shell, activePrompt, config)
        }

      case DownArrow => 
        val newHistoryIdx = renderedEditor.historyIdx - 1
        if (newHistoryIdx == -1) {
          val newState = renderedEditor.setBuffer("").copy(historyIdx = -1)
          loop(newState, shell, activePrompt, config)
        } else if (newHistoryIdx >= 0) {
          val out = HistoryManager.getAtIndex(newHistoryIdx)
          val newState = renderedEditor.setBuffer(out).copy(historyIdx = newHistoryIdx)
          loop(newState, shell, activePrompt, config)
        } else {
          loop(renderedEditor.copy(tabState = TabState.Inactive), shell, activePrompt, config)
        }

      case LeftArrow =>
        val newState = renderedEditor.copy(cursorIdx = Math.max(0, renderedEditor.cursorIdx - 1), tabState = TabState.Inactive)
        loop(newState, shell, activePrompt, config)

      case RightArrow =>
        val newState = renderedEditor.copy(cursorIdx = Math.min(renderedEditor.buffer.length, renderedEditor.cursorIdx + 1), tabState = TabState.Inactive)
        loop(newState, shell, activePrompt, config)

      case End => 
        HistoryManager.getSuggestion(renderedEditor.buffer) match {
          case Some(suggestion) =>
            val newState = renderedEditor.setBuffer(suggestion).copy(tabState = TabState.Inactive)
            loop(newState, shell, activePrompt, config)
          case None => 
            loop(renderedEditor.copy(tabState = TabState.Inactive), shell, activePrompt, config)
        }

      case CharKey(c) => 
        loop(renderedEditor.insertChar(c), shell, activePrompt, config)

      case Escape => 
        val newState = renderedEditor.copy(tabState = TabState.Inactive)
        loop(newState, shell, activePrompt, config)

      case Unknown =>
        System.out.print("\u0007")
        loop(renderedEditor, shell, activePrompt, config)
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
