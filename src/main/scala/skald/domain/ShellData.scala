package skald

import java.nio.file.{Path, Files => JFiles}

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
    case FileNode(path) => field match {
      case "name"   => Some(ShellValue.VString(path.getFileName.toString))
      case "size"   => Some(ShellValue.VLong(JFiles.size(path)))
      case "dir?" => Some(ShellValue.VBool(JFiles.isDirectory(path)))
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

enum ShellValue:
  case VLong(v: Long)
  case VString(s: String)
  case VBool(b: Boolean)
  case VNone

