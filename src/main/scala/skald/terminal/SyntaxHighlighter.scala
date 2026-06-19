package skald

object SyntaxHighlighter {
  def highlight(buffer: String): List[Span] = {
    val parts = buffer.split(" ")

    parts.zipWithIndex.flatMap { (token, index) =>
      val styledToken = if (Builtin.isBuiltin(token)) {
        Span(token, Style(fg = Color.Green))
      } else if (Path.isPath(token)) {
        Span(token, Style(fg = Color.Magenta))
      } else if (FunctionalOp.isFunctionalOp(token)) {
        Span(token, Style(fg = Color.Red))
      } else {
        Span(token, Style(fg = Color.Default))
      }

      if (index < parts.length - 1) List(styledToken, Span(" "))
      else List(styledToken)
    }.toList
  }
}
