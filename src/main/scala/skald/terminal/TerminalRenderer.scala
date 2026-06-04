package skald

import skald.terminal.grid._

object TerminalRenderer {
  def render(buffer: StringBuilder, state: EditorState, prompt: List[Span]): EditorState = {
    System.out.print("\r\u001b[K")

    val termSize = Terminal.getSize()
    val grid = Grid(termSize.columns, termSize.rows)
    
    val endOfPromptX = grid.fill(prompt, 0)
    grid.putString(endOfPromptX, 0, buffer.toString, Style())

    // Cursor positionering
    val absX = endOfPromptX + state.cursorIdx
    val (curY, curX) = (absX / termSize.columns, absX % termSize.columns)
    System.out.print(s"\u001b[${curY + 1};${curX + 1}H")
    System.out.flush()

    state
  }
}
