package skald

import skald.terminal.grid._

object TerminalRenderer {

  def render(editor: EditorState, activePrompt: List[Span], termWidth: Int): Int = {
    val promptLen = activePrompt.map(_.text.length).sum
    val totalChars = promptLen + editor.buffer.length
    val cursorAbsPos = promptLen + editor.cursorIdx

    val physicalRow = if (totalChars > 0 && totalChars % termWidth == 0) {
      (totalChars / termWidth) - 1
    } else {
      totalChars / termWidth
    }

    val targetRow = cursorAbsPos / termWidth
    val targetCol = cursorAbsPos % termWidth

    if (editor.renderedLines > 0) {
      System.out.print(s"\u001b[${editor.renderedLines}A")
    }
    System.out.print("\r") 
    System.out.print("\u001b[J") // Clear everything below

    def renderSpans(spans: List[Span]): Unit = {
      spans.foreach(s => System.out.print(s"\u001b[0m${s.style.fg.ansiCode}${s.text}"))
    }

    renderSpans(activePrompt)
    val highlightedBuffer = SyntaxHighlighter.highlight(editor.buffer)
    renderSpans(highlightedBuffer)

    System.out.print("\r")

    val rowDiff = targetRow - physicalRow
    if (rowDiff > 0) {
      System.out.print(s"\u001b[${rowDiff}B") // Move down
    } else if (rowDiff < 0) {
      System.out.print(s"\u001b[${-rowDiff}A") // Move up
    }

    if (targetCol > 0) {
      System.out.print(s"\u001b[${targetCol}C")
    }
    System.out.flush()
    
    targetRow
  }
}
