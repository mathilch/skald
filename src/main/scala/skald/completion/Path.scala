package skald

object Path {
  def getAllExecutables(): List[String] = {
    val path = sys.env.getOrElse("PATH", "")
    path.split(java.io.File.pathSeparator)
      .map(new java.io.File(_))
      .filter(_.isDirectory)
      .flatMap(_.listFiles())
      .filter(f => f.isFile && f.canExecute)
      .map(_.getName)
      .toList
      .distinct
  }

  def findInPath(name: String): Option[String] = {
    val path = sys.env.getOrElse("PATH", "")
    path.split(java.io.File.pathSeparator)
      .filter(_.nonEmpty)
      .map(new java.io.File(_, name))
      .find(f => f.exists() && f.canExecute)
      .map(_.getAbsolutePath)
  }
}
