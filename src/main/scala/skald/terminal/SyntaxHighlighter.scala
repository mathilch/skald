package skald

import skald.terminal.grid._

object SyntaxHighlighter {
  def highlight(buffer: String): List[Span] = {
    if (buffer.isEmpty) Nil
    else List(Span(buffer, Style(foreground = "\u001b[37m")))
  }
}
