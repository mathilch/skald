package skald 

import java.nio.file.{Path, Paths}

case class ShellEnv(
  state: Map[String, String] = Map.empty,
  cwd: Path = Paths.get(System.getProperty("user.dir"))
) {

  def setVariable(name: String, value: String): ShellEnv =
    ShellEnv(state + (name -> value))

  def getVariable(name: String): Option[String] =
    state.get(name)

  def format(name: String): Either[String, String] =
    getVariable(name) match {
      case Some(value)  => Right(s"declare -- $name=$value")
      case None         => Left(s"declare: $name: not found")
    }
  
  def withCwd(newCwd: Path): ShellEnv =
    this.copy(cwd = newCwd)

}


