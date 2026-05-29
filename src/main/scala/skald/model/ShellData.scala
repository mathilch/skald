package skald

import java.nio.file.{Path, Files => JFiles}
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermissions}
import scala.util.Try
import java.time.ZoneId

enum ShellData:
  case Text(str: String)
  case FileNode(path: Path)
  case FileLine(fileName: String, content: String)
  case ProcessInfo(pid: Int, name: String)
  case GrepMatch(source: Option[String], line: Int, content: String, matchedWord: String)

  def asString: String = this match {
    case Text(str) => str
    case FileNode(path) => path.getFileName.toString
    case FileLine(fn, content) => s"$fn: $content"
    case ProcessInfo(pid, name) => s"[$pid] $name"
    case GrepMatch(source, line, content, matchedWord) =>
      val coloredContent = content.replace(matchedWord, matchedWord.colorize(Color.Yellow))
      source match {
        case Some(fileName) => 
          val coloredFile = s"${Color.Magenta.ansiCode}$fileName${Color.Reset.ansiCode}"
          val coloredLine = s"${Color.Green.ansiCode}line $line${Color.Reset.ansiCode}"
          
          s"$coloredFile: $coloredLine: $coloredContent"
        case None           => coloredContent
    }
  }

  def getProperty(field: String): Option[ShellValue] = this match {
    case FileNode(path) => 
      val attrsOpt = Try(JFiles.readAttributes(path, classOf[BasicFileAttributes])).toOption
      val fileName = path.getFileName.toString
      
      field match {
        case "name"   => Some(ShellValue.VString(fileName))
        case "size"   => 
          val size = attrsOpt.map(_.size()).getOrElse(JFiles.size(path))
          Some(ShellValue.VLong(size))
        case "dir?"   => 
          val isDir = attrsOpt.map(_.isDirectory).getOrElse(JFiles.isDirectory(path))
          Some(ShellValue.VBool(isDir))
          
        case "file?"  => 
          val isFile = attrsOpt.map(_.isRegularFile).getOrElse(JFiles.isRegularFile(path))
          Some(ShellValue.VBool(isFile))
        case "exec?"  => Some(ShellValue.VBool(JFiles.isExecutable(path)))
        case "ext"    => 
          val dotIdx = fileName.lastIndexOf(".")
          val ext = if (dotIdx > 0 && dotIdx < fileName.length() - 1) fileName.substring(dotIdx + 1) else ""
          Some(ShellValue.VString(ext))
        case "hidden?" =>
          Some(ShellValue.VBool(fileName.startsWith(".")))
        case "modified" =>
          attrsOpt.map { attrs => 
            val date = attrs.lastModifiedTime()
              .toInstant()
              .atZone(ZoneId.systemDefault())
              .toLocalDate()
            ShellValue.VDate(date)
          }
        case "created" =>
          attrsOpt.map { attrs => 
            val date = attrs.creationTime()
              .toInstant()
              .atZone(ZoneId.systemDefault())
              .toLocalDate()
            ShellValue.VDate(date)
          }

        case "accessed" =>
          attrsOpt.map { attrs => 
            val date = attrs.lastAccessTime()
              .toInstant()
              .atZone(ZoneId.systemDefault())
              .toLocalDate()
            ShellValue.VDate(date)
          }
        case "permissions" =>
          val permsStr = Try {
            PosixFilePermissions.toString(JFiles.getPosixFilePermissions(path))
          }.getOrElse("---------") // Fallback hvis koden kører på Windows, som ikke bruger POSIX
          Some(ShellValue.VString(permsStr))
        case _        => None
      }
    case ProcessInfo(pid, name) => field match {
      case "name" => Some(ShellValue.VString(name))
      case "pid"  => Some(ShellValue.VLong(pid.toLong))
      case _ => None
    }
    case Text(str) => field match {
      case "length" => Some(ShellValue.VLong(str.length.toLong))
      case _ => None
    }

    case FileLine(fileName, content) => field match {
      case "file" | "filename" | "name" => Some(ShellValue.VString(fileName))
      case "content" | "text"           => Some(ShellValue.VString(content))
      case "length"                     => Some(ShellValue.VLong(content.length.toLong))
      
      case "ext" => 
        val dotIdx = fileName.lastIndexOf(".")
        val ext = if (dotIdx > 0 && dotIdx < fileName.length() - 1) fileName.substring(dotIdx + 1) else ""
        Some(ShellValue.VString(ext))
        
      case _ => None
    }

    case GrepMatch(source, line, content, matchedWord) => field match {
      case "source" | "file"  => source.map(ShellValue.VString(_))
      case "line"             => Some(ShellValue.VLong(line.toLong))
      case "match" | "word"   => Some(ShellValue.VString(matchedWord))
      case "content" | "text" => Some(ShellValue.VString(content))
      case "length"           => Some(ShellValue.VLong(content.length.toLong))
      case _                  => None
    }

  }

