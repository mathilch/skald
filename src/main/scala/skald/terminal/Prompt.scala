package skald 

import java.io.{File}
import java.nio.file.Path
import scala.sys.process._

enum Prompt:
  case Static(text: StyledText)
  case CurrentDir
  case GitBranch

object PromptEngine {
  def render(env: ShellEnv): String = {
    val cwd = env.cwd 
    val dirName = if (cwd.toString == "/") "/" else cwd.getFileName.toString
    
    val gitBranch = getGitBranch(cwd) 
    
    // Byg de enkelte dele med StyledText
    val dirStyled = StyledText(s"~/$dirName", List(Color.Bold, Color.Blue)).render
    val gitStyled = gitBranch
      .map(b => s" ${StyledText(s"($b)", List(Color.Green)).render}")
      .getOrElse("")
    val arrowStyled = StyledText.plain(" > ").render
    
    // Sæt strengen sammen
    s"$dirStyled$gitStyled$arrowStyled"
  }

  private def getGitBranch(cwd: Path): Option[String] = {
    try {
      val io = Process(Seq("git", "branch", "--show-current"), cwd.toFile)
        .lazyLines_!(ProcessLogger(_ => ()))
        
      val branch = io.headOption 
      branch.filter(_.trim.nonEmpty)
    } catch {
      case _: Exception => None
    }
  }
}
