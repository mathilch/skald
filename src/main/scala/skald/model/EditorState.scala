package skald

case class EditorState(
  buffer: String = "",
  cursorIdx: Int = 0,
  historyIdx: Int = -1,
  tabState: TabState = TabState.Inactive,
  renderedLines: Int = 0
) {
  def insertChar(c: Char): EditorState = {
    val (left, right) = buffer.splitAt(cursorIdx)
    this.copy(
      buffer = left + c + right,
      cursorIdx = cursorIdx + 1,
      historyIdx = -1,
      tabState = TabState.Inactive
    )
  }

  def backspace: EditorState = {
    if (cursorIdx > 0) {
      val (left, right) = buffer.splitAt(cursorIdx)
      this.copy(buffer = left.init + right, cursorIdx = cursorIdx - 1, tabState = TabState.Inactive)
    } else this
  }
  
  def setBuffer(text: String): EditorState = {
    this.copy(buffer = text, cursorIdx = text.length, tabState = TabState.Inactive)
  }

  def updateBuffer(text: String): EditorState = {
    this.copy(buffer = text, cursorIdx = text.length)
  }
}
