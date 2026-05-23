package skald

import java.nio.file.{Path, Files => JFiles}
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermissions}
import scala.util.Try
import java.time.ZoneId

enum ShellData:
  case Text(str: String)
  case FileNode(path: Path)
  case ProcessInfo(pid: Int, name: String)

  def asString: String = this match {
    case Text(str) => str
    case FileNode(path) => path.getFileName.toString
    case ProcessInfo(pid, name) => s"[$pid] $name"
  }

  def getProperty(field: String): Option[ShellValue] = this match {
    case FileNode(path) => 
      val attrsOpt = Try(JFiles.readAttributes(path, classOf[BasicFileAttributes])).toOption
      val fileName = path.getFileName.toString
      
      field match {
        case "name"   => Some(ShellValue.VString(fileName))
        case "size"   => Some(ShellValue.VLong(JFiles.size(path)))
        case "dir?"   => Some(ShellValue.VBool(JFiles.isDirectory(path)))
        case "file?"  => Some(ShellValue.VBool(JFiles.isRegularFile(path)))
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
  }

