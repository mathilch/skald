package skald 

object Parser {
  def parse(tokens: List[String]): Option[Command] = {
    if (tokens.last == "&") {
      val cmd = tokens.init
      return parse(cmd).map(process => Subprocess(process))
    }

    val redirectTokens = Set(">", "1>", "2>", ">>", "1>>", "2>>")
    val rIdx = tokens.indexWhere(redirectTokens.contains)
       
    if (rIdx != -1) {
      // Hvis 1> eller > eksistere
      val left = tokens.take(rIdx)
      val right = tokens.lift(rIdx + 1).getOrElse("")
      val op = tokens(rIdx)

      parse(left).map { cmd =>
        op match {
          case "1>>" | ">>" => AppendStdout(cmd, right)
          case "2>>" => AppendStderr(cmd, right)
          case "2>" => RedirectStderr(cmd, right)
          case _ => RedirectStdout(cmd, right)
        }
      }
    } else {
      val pipeIdx = tokens.lastIndexOf("|")
      if (pipeIdx != -1) {
        
        val segments = tokens.foldLeft(List(List.empty[String])) { (acc, token) =>
          token match {
            case "|" => List.empty[String] :: acc
            case t => (t :: acc.head) :: acc.tail
          }
        }.map(_.reverse).reverse

        Util.sequence(segments.map(parse)).map(Pipeline.apply)


        // val (lTokens, pipeAndRight) = tokens.splitAt(pipeIdx)
        // val rTokens = pipeAndRight.drop(1)
        //
        // for {
        //   leftCmd <- parse(lTokens)
        //   rightCmd <- parse(rTokens)
        // } yield Pipeline(leftCmd, rightCmd)
        
      } else {
        // Hvis > ikke eksistere
        tokens match {
          case Nil => None
          case head :: tail =>
            Builtin.fromString(head) match {
              case Some(b) => b match {
                case Builtin.Exit => Some(Exit)
                case Builtin.Echo => Some(Echo(tail))
                case Builtin.Pwd => Some(Pwd())
                case Builtin.Type => Some(Type(tail))
                case Builtin.Cd => Some(Cd(tail))
                case Builtin.Complete =>
                  tail match {
                    case "-p" :: cmd :: Nil =>
                      Some(Complete(PrintSpec(cmd)))
                    case "-C" :: path :: cmd :: Nil =>
                      Some(Complete(RegisterSpec(path, cmd)))
                    case "-r" :: cmd :: Nil =>
                      Some(Complete(UnregisterSpec(cmd)))
                    case _ => None
                  }
                case Builtin.Jobs => Some(Jobs())
                case Builtin.History => 
                  tail match {
                    case "-r" :: file :: Nil => Some(History(ReadFromFile(file)))
                    case "-w" :: file :: Nil => Some(History(WriteToFile(file)))
                    case "-a" :: file :: Nil => Some(History(AppendToFile(file)))
                    case n :: Nil => n.toIntOption match {
                      case Some(number) => Some(History(nHistory(number)))
                      case None => None
                    }
                    case Nil => Some(History(ShowAll))
                    case _ => None
                  }
                case Builtin.Declare =>
                  tail match {
                    case "-p" :: variable :: Nil => Some(Declare(PrintVariable(variable)))
                    case assignment :: Nil if assignment.contains("=") =>
                      val Array(name, value) = assignment.split("=")
                      System.out.print(s"Printing: $name = $value")
                      Some(Declare(AssignVariable(name, value)))
                      
                    case _ => None
                  }

              }
              case None => Some(External(head, tail))
            }
        }
      }

    }
  }
}

