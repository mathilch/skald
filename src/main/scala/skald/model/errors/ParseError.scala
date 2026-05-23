package skald

enum ParseError:
  case InvalidSyntax(msg: String)
  case UnknownOperator(op: String)
  case MissingArguments(command: String)

  def printError: String = this match {
    case InvalidSyntax(msg)     => msg
    case UnknownOperator(msg)   => msg
    case MissingArguments(msg)  => msg
  }
