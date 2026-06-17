package skald

enum TabState:
  case Inactive
  case Active(prefix: String, options: Vector[String], currentIdx: Int)
