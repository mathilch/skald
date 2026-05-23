package skald 

import Command._
import RedirectionType._
import RedirectionMode._
import CompleteFlag._
import DeclareFlag._
import HistoryFlag._

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
        val (target, mode) = op match {
          case "1>>" | ">>" => (Stdout, Append)
          case "2>>"        => (Stderr, Append)
          case "2>"         => (Stderr, Overwrite)
          case _            => (Stdout, Overwrite) // > eller 1>
        }
        Redirect(cmd, target, mode, right)
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

      } else {
        // Hvis > ikke eksistere
        tokens match {
          case Nil => None
          case head :: tail =>
            Builtin.fromString(head) match {
              case Some(b) => b match {
                case Builtin.Exit => Some(Exit)
                case Builtin.Echo => Some(Echo(tail))
                case Builtin.Pwd => Some(Pwd)
                case Builtin.Type => Some(Type(tail))
                case Builtin.Cd => Some(Cd(tail))
                case Builtin.Ls => Some(Ls)
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
                case Builtin.Jobs => Some(Jobs)
                case Builtin.History => 
                  tail match {
                    case "-r" :: file :: Nil => Some(History(ReadFromFile(file)))
                    case "-w" :: file :: Nil => Some(History(WriteToFile(file)))
                    case "-a" :: file :: Nil => Some(History(AppendToFile(file)))
                    case n :: Nil => n.toIntOption match {
                      case Some(number) => Some(History(NHistory(number)))
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
              case None => FunctionalOp.fromString(head) match {
                case Some(op) => op match {
                  case FunctionalOp.Filter  => 
                    parseExpr(tail) match {
                      case Some(expr) => Some(Filter(expr))
                      case None => None
                    }
                  case FunctionalOp.Map     => 
                    parseExpr(tail) match {
                      case Some(expr) => Some(Map(expr))
                      case None => None
                    }
                  // case FunctionalOp.Sort    => 
                  //   parseExpr(tail) match {
                  //     case Some(expr) => Some(Sort(expr))
                  //     case None => None
                  //   }
                }
              
                case _ => Some(External(head, tail))
              }
            }
        }
      }

    }
  }

  def parseExpr(tokens: List[String]): Option[Expr] = tokens match {

    case prop :: Nil if prop.startsWith("_.") =>
      Some(Expr.PropAccess(prop.stripPrefix("_.")))

    case prop :: op :: value :: Nil if prop.startsWith("_.") =>
      val field = prop.stripPrefix("_.")
      val left = Expr.PropAccess(field)

      val right = parseByteSize(value) match {
        case Some(num) => Expr.LitInt(num)
        case None      => value.toBooleanOption match {
          case Some(b) => Expr.LitBool(b)
          case None => Expr.LitStr(value)
        }

          
      }

      op match {
        case "gt" => Some(Expr.GreaterThan(left, right))
        case "eq" => Some(Expr.Equals(left, right))
        case _ => None
      }
    case _ => None
  }

  def parseByteSize(value: String): Option[Long] = {
    // Fanger et valgfrit minus, derefter tal, og til sidst valgfri bogstaver
    val sizeRegex = """^(-?\d+)([a-zA-Z]+)?$""".r

    value match {
      case sizeRegex(numStr, null) => 
        // Der var ingen enhed (f.eks. "100"), så det er bare bytes
        numStr.toLongOption

      case sizeRegex(numStr, suffix) =>
        // Vi har både tal og enhed (f.eks. "100", "mib")
        for {
          num <- numStr.toLongOption
          multiplier <- suffix.toLowerCase match {
            case "b"   => Some(1L)
            // SI standard (powers of 10)
            case "kb"  => Some(1000L)
            case "mb"  => Some(1000000L)
            case "gb"  => Some(1000000000L)
            case "tb"  => Some(1000000000000L)
            // IEC standard (powers of 2)
            case "kib" => Some(1024L)
            case "mib" => Some(1048576L)
            case "gib" => Some(1073741824L)
            case "tib" => Some(1099511627776L)
            case _     => None // Ukendt enhed (f.eks. "100xx")
          }
        } yield num * multiplier

      case _ => None // Matcher slet ikke (f.eks. ren tekst som "foo")
    }
  }
}

