package skald

enum Builtin(val name: String):
  case Echo     extends Builtin("echo")
  case Exit     extends Builtin("exit")
  case Type     extends Builtin("type")
  case Pwd      extends Builtin("pwd")
  case Cd       extends Builtin("cd")
  case Complete extends Builtin("complete")
  case Jobs     extends Builtin("jobs")
  case History  extends Builtin("history")
  case Declare  extends Builtin("declare")

object Builtin:
  def fromString(s: String): Option[Builtin] =
    values.find(_.name == s)
  def isBuiltin(s: String): Boolean =
    values.exists(_.name == s)
