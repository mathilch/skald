package skald

object TerminalRenderer {
  def render(buffer: StringBuilder, state: EditorState, prompt: String): EditorState = {
    val currentInput = buffer.toString
    val suggestionOpt = HistoryManager.getSuggestion(currentInput)
    val termSize = Terminal.getSize()

    // 1. Fjern ANSI-farvekoder for at beregne den reelle synlige længde af prompten:
    val visiblePromptLen = prompt.replaceAll("\u001b\\[[0-9;]*[a-zA-Z]", "").length
    val totalBufferLength = visiblePromptLen + currentInput.length

    val totalLines = math.ceil(totalBufferLength.toDouble / termSize.columns).toInt
    
    if (state.renderedLines > 1) {
      System.out.print(s"\u001b[${state.renderedLines - 1}A")
    }

    System.out.print("\r")
    System.out.print("\u001b[J")
    System.out.print(s"$prompt$buffer")

    if (suggestionOpt.isDefined) {
      val suggestion = suggestionOpt.get
      val remainder = suggestion.substring(currentInput.length)
      System.out.print(s"\u001b[90m$remainder\u001b[0m")
    }

    val cursorAbsolutePos = visiblePromptLen + state.cursorIdx
    val cursorRow = cursorAbsolutePos / termSize.columns
    val cursorCol = (cursorAbsolutePos % termSize.columns) + 1

    val linesToMoveUp = (totalLines - 1) - cursorRow 

    if (linesToMoveUp > 0) {
      System.out.print(s"\u001b[${linesToMoveUp}A")
    }

    System.out.print(s"\r\u001b[${cursorCol}G")
    System.out.flush()
    

    state.copy(renderedLines = totalLines)
  }
}
