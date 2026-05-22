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


sealed trait Command
case object Exit extends Command
case class Echo(args: List[String]) extends Command
case class Pwd() extends Command
case class Type(args: List[String]) extends Command 
case class Cd(args: List[String]) extends Command
case class Complete(arg: CompleteFlag) extends Command
case class Jobs() extends Command
case class History(arg: HistoryFlag) extends Command
case class Declare(arg: DeclareFlag) extends Command

sealed trait DeclareFlag
case class PrintVariable(variable: String) extends DeclareFlag
case class AssignVariable(variable: String, value: String) extends DeclareFlag

sealed trait HistoryFlag
case object ShowAll extends HistoryFlag
case class nHistory(n: Int) extends HistoryFlag
case class ReadFromFile(file: String) extends HistoryFlag
case class WriteToFile(file: String) extends HistoryFlag
case class AppendToFile(file: String) extends HistoryFlag

sealed trait CompleteFlag
case class PrintSpec(cmd: String) extends CompleteFlag
case class RegisterSpec(path: String, cmd: String) extends CompleteFlag
case class UnregisterSpec(cmd: String) extends CompleteFlag


//case class Execute(cmd: Builtin, args: List[String]) extends Command
case class External(name: String, args: List[String]) extends Command
case class Pipeline(commands: List[Command]) extends Command

sealed trait RedirectionType
case object Stdout extends RedirectionType
case object Stderr extends RedirectionType

sealed trait RedirectionMode
case object Overwrite extends RedirectionMode
case object Append extends RedirectionMode

case class Redirect(
  cmd: Command, 
  target: RedirectionType, 
  mode: RedirectionMode, 
  targetFile: String) extends Command

case class Subprocess(cmd: Command) extends Command


