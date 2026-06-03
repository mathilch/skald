package skald 

import java.nio.file.Path
import scala.sys.process._

object PromptEngine {
  // når der i conf står \green{noget tekst} f.eks.
  private val colorRegex = """\\([a-zA-Z]+)\{([^}]*)\}""".r

  def render(env: ShellEnv, config: SkaldConfig): String = {

    val gitBranch = GitStatus.fromPath(env.cwd) match {
      case GitStatus.Branch(name)   => config.gitFormat.replace("%s", name)
      case GitStatus.NotARepository => ""
    }

    val currentDirStr = formatDir(env.cwd.toString, config.dirDepth)
    
    val basePrompt = config.promptTemplate
      .replace("%w", currentDirStr)
      .replace("%b", gitBranch)

    val rendered = colorRegex.replaceAllIn(basePrompt, matchData => {
      val colorName = matchData.group(1)
      val innerText = matchData.group(2)

      Color.fromString(colorName) match {
        case Some(color) =>
          s"${color.ansiCode}$innerText${Color.Reset.ansiCode}"
        case None => innerText
      }
    })

    rendered + " "
  }

  private def formatDir(path: String, deptOpt: Option[Int]): String = {
    deptOpt match {
      case Some(depth) if depth > 0 =>
        val parts = path.split("/").filter(_.nonEmpty)
        if (parts.length <= depth) path
        else "/" + parts.takeRight(depth).mkString("/")
      case _ => path
    }
  }
}
