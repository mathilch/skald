package skald 

import java.nio.file.{Path, Paths}

case class ShellEnv(
  state: Map[String, String] = Map.empty,
  aliases: Map[String, String] = Map.empty,
  cwd: Path = Paths.get(System.getProperty("user.dir"))
) {

  def setVariable(name: String, value: String): ShellEnv =
    this.copy(state = this.state + (name -> value))

  def getVariable(name: String): Option[String] =
    state.get(name)

  def formatVariable(name: String): Either[String, String] =
    getVariable(name) match {
      case Some(value)  => Right(s"declare -- $name=$value")
      case None         => Left(s"declare: $name: not found")
    }

  def setAlias(name: String, value: String): ShellEnv =
    this.copy(aliases = this.aliases + (name -> value))

  def getAlias(name: String): Option[String] =
    aliases.get(name)

  def removeAlias(name: String): Option[ShellEnv] =
    getAlias(name) match {
      case Some(_) => Some(this.copy(aliases = this.aliases - name))
      case None => None
    }
  
  def withCwd(newCwd: Path): ShellEnv =
    this.copy(cwd = newCwd)

}


