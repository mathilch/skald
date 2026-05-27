package skald

enum FileError:
  case CannotReadFile(msg: String)

  def printError: String = this match {
    case CannotReadFile(msg) => msg
  }
