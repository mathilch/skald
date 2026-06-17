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

  def parse(tokens: List[String], aliases: Map[String, String]): Result[ParseError, Command] = {
    if (tokens.lastOption.contains("&")) {
      parse(tokens.init, aliases).map(Subprocess.apply)
    } else {
      parsePipeline(ParserState(tokens), aliases)
    }
  }

  private def parsePipeline(state: ParserState, aliases: Map[String, String]): Result[ParseError, Command] = {
    val pipeIdx = state.tokens.indexOf("|")
    if (pipeIdx != -1) {
      val (left, right) = state.tokens.splitAt(pipeIdx)
      for {
        leftCmd <- parseRedirect(ParserState(left), aliases)
        restCmd <- parsePipeline(ParserState(right.tail), aliases)
      } yield Pipeline(List(leftCmd) ++ (restCmd match { case Pipeline(cmds) => cmds; case c => List(c)}))
    } else {
      parseRedirect(state, aliases)
    }
  }

  private def parseRedirect(state: ParserState, aliases: Map[String, String]): Result[ParseError, Command] = {
    val redirectTokens = Set(">", "1>", "2>", ">>", "1>>", "2>>")
    val rIdx = state.tokens.indexWhere(redirectTokens.contains)

    if (rIdx != -1) {
      val (left, right) = state.tokens.splitAt(rIdx)
      val op = right.head
      val targetFile = right.tail.headOption.getOrElse("")

      parseSimpleCommand(ParserState(left), aliases).map { cmd =>
        val (target, mode) = op match {
          case "1>>" | ">>" => (Stdout, Append)
          case "2>>"        => (Stderr, Append)
          case "2>"         => (Stderr, Overwrite)
          case _            => (Stdout, Overwrite) // > eller 1>
        }
        Redirect(cmd, target, mode, targetFile)
      }

    } else {
      parseSimpleCommand(state, aliases)
    }
  }

  private def parseSimpleCommand(state: ParserState, aliases: Map[String, String]): Result[ParseError, Command] = {
    state.peek match {
      case None => Fail(ParseError.MissingArguments("Empty"))
      case Some(head) =>
        val resolved = aliases.getOrElse(head, head)
        val args = state.tokens.tail

        if (Builtin.isBuiltin(resolved)) parseBuiltin(resolved, args)
        else if (FunctionalOp.isFunctionalOp(resolved)) parseFunctionalOp(resolved, ParserState(args))
      //else if (Interactive.isInteractive(resolved)) Success(Command.Interactive(resolved, args))
        else Success(Command.External(resolved, args))
    }
  }

  private def parseBuiltin(name: String, tail: List[String]): Result[ParseError, Command] = name match {
    case "exit"     => Success(Exit)
    case "echo"     => Success(Echo(tail))
    case "pwd"      => Success(Pwd)
    case "type"     => Success(Type(tail))
    case "cd"       => Success(Cd(tail))
    case "ls"       => Success(Ls(tail))
    case "jobs"     => Success(Jobs)
    case "cat"      => Success(Cat(tail))
    case "complete" => parseComplete(tail)
    case "history"  => parseHistory(tail)
    case "grep"     => parseGrep(tail)
    case "declare"  => parseDeclare(tail)
    case "export"   => parseExport(tail)
    case "alias"    => parseAlias(tail)
    case "unalias"  => parseUnalias(tail)
    case "source"   => parseSource(tail)
  }

  private def parseHistory(tail: List[String]): Result[ParseError, Command] = tail match {
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

  private def parseComplete(tail: List[String]): Result[ParseError, Command] = tail match {
    case "-p" :: cmd :: Nil => Success(Complete(PrintSpec(cmd)))
    case "-C" :: path :: cmd :: Nil => Success(Complete(RegisterSpec(path, cmd)))
    case "-r" :: cmd :: Nil => Success(Complete(UnregisterSpec(cmd)))
    case _ => Fail(ParseError.MissingArguments(s"Unrecognized arguments for Complete"))
  }

  private def parseGrep(tail: List[String]): Result[ParseError, Command] = tail match {
    case Nil => Fail(ParseError.MissingArguments(s"Unrecognized argument for Grep"))
    case word :: files => Success(Grep(word, files))
  }

  private def parseDeclare(tail: List[String]): Result[ParseError, Command] = tail match {
    case "-p" :: variable :: Nil => Success(Declare(PrintVariable(variable)))
    case assignment :: Nil if assignment.contains("=") =>
      val Array(name, value) = assignment.split("=", 2)
      Success(Declare(AssignVariable(name, value)))
    case _ => Fail(ParseError.MissingArguments(s"Unrecognized arguments for Declare"))
  }

  private def parseExport(tail: List[String]): Result[ParseError, Command] = tail match {
    case assignment :: Nil if assignment.contains("=") =>
      val Array(name, value) = assignment.split("=", 2)
      Success(Declare(AssignVariable(name, value)))
    case _ => Fail(ParseError.MissingArguments(s"Unrecognized arguments for Export"))
  }

  private def parseAssignment(tail: List[String]): Result[ParseError, Command] = tail match {
    case assignment :: Nil if assignment.contains("=") =>
      val Array(name, value) = assignment.split("=", 2)
      Success(Declare(AssignVariable(name, value)))
    case _ => Fail(ParseError.MissingArguments("Ugyldig assignment format"))
  }

  private def parseAlias(tail: List[String]): Result[ParseError, Command] = tail match {
    case Nil => Success(Alias(PrintAll))
    case assignment :: Nil if assignment.contains("=") =>
      val Array(name, value) = assignment.split("=", 2)
      val cleanValue = value.stripPrefix("\"").stripSuffix("\"").stripPrefix("\'").stripSuffix("\'")
      Success(Alias(AssignAlias(name, cleanValue)))
    case _ => Fail(ParseError.InvalidSyntax(s"use: alias name=\"value\" to create an alias"))
  }

  private def parseUnalias(tail: List[String]): Result[ParseError, Command] = tail match {
    case name :: Nil => Success(Unalias(name))
    case _ => Fail(ParseError.InvalidSyntax("use: unalias name"))
  }

  private def parseSource(tail: List[String]): Result[ParseError, Command] = tail match {
    case file :: Nil => Success(Source(file))
    case Nil => Fail(ParseError.MissingArguments("source requires a filesname"))
    case _ => Fail(ParseError.InvalidSyntax("source requires exactly one filename"))
  }

  private def parseFunctionalOp(name: String, state: ParserState): Result[ParseError, Command] = {
    parseExpr(state).flatMap { expr =>
      name match {
        case "filter" => Success(Filter(expr))
        case "map"    => Success(MapCmd(expr))
        case "sort"   => Success(Sort(expr, false))
        case unknown  => Fail(ParseError.InvalidSyntax(s"Not implemented yet: $unknown"))
      }
    }
  }


  private def parseExpr(state: ParserState): Result[ParseError, Expr] = {
    val (prop, state1) = state.consume
    
    if (!prop.startsWith("_.")) return Fail(ParseError.InvalidSyntax(s"Prop expected, instead got: $prop"))
    
    val left = Expr.PropAccess(prop.stripPrefix("_."))
    state1.peek match {
      case None => Success(left)
      case Some(op) =>
        val (_, state2) = state1.consume
        if (state2.isEmpty) {
          Fail(ParseError.MissingArguments(s"Value required for op: '$op'"))
        } else {
          val (value, _) = state2.consume
          val right = parseLiteral(value)
          
          op match {
            case "gt" => Success(Expr.GreaterThan(left, right))
            case "lt" => Success(Expr.LesserThan(left, right))
            case "eq" => Success(Expr.Equals(left, right))
            case _    => Fail(ParseError.UnknownOperator(s"Unknown operator: $op"))
          }
        }
    }
  }

  private def parseLiteral(value: String): Expr = {
    parseByteSize(value) match {
      case Some(num) => Expr.LitInt(num)
      case None => value.toBooleanOption match {
        case Some(b) => Expr.LitBool(b)
        case None    => Expr.LitStr(value)
      }
    }
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

