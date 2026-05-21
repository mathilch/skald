package skald

import java.nio.file.Path

enum ShellData:
  case Text(str: String)
  case FileNode(path: Path)
  case ProcessInfo(pid: Int, name: String)
