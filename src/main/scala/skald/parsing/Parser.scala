package skald 

import Command.{Map => MapCmd, _}
import RedirectionType._
import RedirectionMode._
import CompleteFlag._
import DeclareFlag._
import HistoryFlag._
import Result._
import AliasFlag._

object Parser {
  def parse(tokens: List[String], aliases: Map[String, String] = Map.empty): Result[ParseError, Command] = {
    if (tokens.last == "&") {
      val cmd = tokens.init
      return parse(cmd, aliases).map(process => Subprocess(process))
    }

    val redirectTokens = Set(">", "1>", "2>", ">>", "1>>", "2>>")
    val rIdx = tokens.indexWhere(redirectTokens.contains)
       
    if (rIdx != -1) {
      // Hvis 1> eller > eksistere
      val left = tokens.take(rIdx)
      val right = tokens.lift(rIdx + 1).getOrElse("")
      val op = tokens(rIdx)

      parse(left, aliases).map { cmd =>
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

        return segments.map(s => parse(s, aliases)).sequence.map(Pipeline.apply)
  
      } else {
        // Hvis > ikke eksistere
        tokens match {
          case Nil => Fail(ParseError.MissingArguments(s"Nothing"))
          case h :: t =>
            val resolvedTokens = aliases.get(h) match {
              case Some(aliasValue) => Lexer.tokenizeInput(aliasValue) ++ t
              case None => tokens
            }

            resolvedTokens match {
              case Nil => Fail(ParseError.MissingArguments(s"Empty alias"))
              case head :: tail =>
                Builtin.fromString(head) match {
                  case Some(b) => b match {
                    case Builtin.Exit => Success(Exit)
                    case Builtin.Echo => Success(Echo(tail))
                    case Builtin.Pwd  => Success(Pwd)
                    case Builtin.Type => Success(Type(tail))
                    case Builtin.Cd   => Success(Cd(tail))
                    case Builtin.Ls   => Success(Ls)
                    case Builtin.Complete =>
                      tail match {
                        case "-p" :: cmd :: Nil => Success(Complete(PrintSpec(cmd)))
                        case "-C" :: path :: cmd :: Nil => Success(Complete(RegisterSpec(path, cmd)))
                        case "-r" :: cmd :: Nil => Success(Complete(UnregisterSpec(cmd)))
                        case _ => Fail(ParseError.MissingArguments(s"Unrecognized arguments for Complete"))
                      }
                    case Builtin.Jobs => Success(Jobs)
                    case Builtin.History =>
                      tail match {
                        case "-r" :: file :: Nil => Success(History(ReadFromFile(file)))
                        case "-w" :: file :: Nil => Success(History(WriteToFile(file)))
                        case "-a" :: file :: Nil => Success(History(AppendToFile(file)))
                        case n :: Nil => n.toIntOption match {
                          case Some(number) => Success(History(NHistory(number)))
                          case None => Fail(ParseError.MissingArguments("Unrecognized argument for History"))
                        }
                        case Nil => Success(History(ShowAll))
                        case _ => Fail(ParseError.MissingArguments("Unrecognized arguments for History"))
                      }
                    case Builtin.Declare =>
                      tail match {
                        case "-p" :: variable :: Nil => Success(Declare(PrintVariable(variable)))
                        case assignment :: Nil if assignment.contains("=") =>
                          val Array(name, value) = assignment.split("=", 2)
                          Success(Declare(AssignVariable(name, value)))
                        case _ => Fail(ParseError.MissingArguments(s"Unrecognized arguments for Declare"))
                      }
                    case Builtin.Alias =>
                      tail match {
                        case Nil => Success(Alias(PrintAll))
                        case assignment :: Nil if assignment.contains("=") =>
                          val Array(name, value) = assignment.split("=", 2)
                          val cleanValue = value.stripPrefix("\"").stripSuffix("\"").stripPrefix("\'").stripSuffix("\'")
                          Success(Alias(AssignAlias(name, cleanValue)))
                        case _ => Fail(ParseError.InvalidSyntax(s"use: alias name=\"value\" to create an alias"))
                      }
                    case Builtin.Unalias =>
                      tail match {
                        case name :: Nil => Success(Unalias(name))
                        case _ => Fail(ParseError.InvalidSyntax("use: unalias name"))
                      }

                    case Builtin.Source =>
                      tail match {
                        case file :: Nil => Success(Source(file))
                        case Nil => Fail(ParseError.MissingArguments("source requires a filesname"))
                        case _ => Fail(ParseError.InvalidSyntax("source requires exactly one filename"))
                      }
                  } 

                  case None => FunctionalOp.fromString(head) match {
                    case Some(op) => op match {
                      case FunctionalOp.Filter =>
                        parseExpr(tail) match {
                          case Success(expr) => Success(Filter(expr))
                          case Fail(err)     => Fail(err)
                        }
                      case FunctionalOp.Map => // Bruger din MapCmd her
                        parseExpr(tail) match {
                          case Success(expr) => Success(skald.Command.Map(expr))
                          case Fail(err)     => Fail(err)
                        }
                      case FunctionalOp.Sort =>
                        parseExpr(tail) match {
                          case Success(expr) => Success(Sort(expr, false))
                          case Fail(err)     => Fail(err)
                        }
                    }
                    case None => Success(External(head, tail))
                  } 
                } 
            } 
        } 
      }

    }
  }

  def parseExpr(tokens: List[String]): Result[ParseError, Expr] = tokens match {

    case prop :: Nil if prop.startsWith("_.") =>
      Success(Expr.PropAccess(prop.stripPrefix("_.")))

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
        case "gt" => Success(Expr.GreaterThan(left, right))
        case "lt" => Success(Expr.LesserThan(left, right))
        case "eq" => Success(Expr.Equals(left, right))
        case _ => Fail(ParseError.UnknownOperator(s"Unknown operatoe: $op. Expected gt, lt or eq"))
      }

    case _ => Fail(ParseError.InvalidSyntax(s"Wrong syntax"))
  }

  private def parseByteSize(value: String): Option[Long] = {
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

