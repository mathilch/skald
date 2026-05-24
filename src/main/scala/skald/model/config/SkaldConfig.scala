package skald

case class SkaldConfig(
  promptTemplate: String = """\green{skald}:\blue{%w}%b $ """,
  gitFormat: String = """ \magenta{%s}""",
  dirDepth: Option[Int] = Some(1),
  cursorStyle: CursorStyle = CursorStyle.BlinkIBeam
)   
