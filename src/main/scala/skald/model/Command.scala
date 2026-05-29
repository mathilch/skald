package skald

enum Command:
  case Exit
  case Echo(args: List[String])
  case Pwd
  case Type(args: List[String])
  case Cd(args: List[String])
  case Ls
  case Cat(files: List[String])
  case Grep(pattern: String, files: List[String])
  case Complete(arg: CompleteFlag)
  case Jobs
  case History(arg: HistoryFlag)

  case Declare(arg: DeclareFlag)
  case Export(name: String, value: String)

  case External(name: String, args: List[String])
  case Pipeline(commands: List[Command])
  case Redirect(cmd: Command, target: RedirectionType, mode: RedirectionMode, targetFile: String)
  case Subprocess(cmd: Command)

  case Filter(expr: Expr)
  case Map(expr: Expr)
  case Sort(expr: Expr, descending: Boolean = false)

  case Alias(arg: AliasFlag)
  case Unalias(name: String)
  case Source(file: String)

enum AliasFlag:
  case PrintAll
  case AssignAlias(variable: String, value: String)

enum DeclareFlag:
  case PrintVariable(variable: String)
  case AssignVariable(variable: String, value: String)

enum HistoryFlag:
  case ShowAll
  case NHistory(n: Int) 
  case ReadFromFile(file: String)
  case WriteToFile(file: String)
  case AppendToFile(file: String)

enum CompleteFlag:
  case PrintSpec(cmd: String)
  case RegisterSpec(path: String, cmd: String)
  case UnregisterSpec(cmd: String)

enum RedirectionType:
  case Stdout, Stderr

enum RedirectionMode:
  case Overwrite, Append
