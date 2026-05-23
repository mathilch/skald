package skald

case class EditorState(
  cursorIdx: Int = 0,
  historyIdx: Int = -1,
  tabCount: Int = 0
)
