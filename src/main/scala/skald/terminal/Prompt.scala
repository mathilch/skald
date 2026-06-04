package skald 

import skald.terminal.grid.Span

import java.nio.file.Path
import scala.sys.process._
import skald.terminal.grid.Style
import scala.util.matching.Regex
import scala.annotation.tailrec

object PromptEngine {
  // når der i conf står \green{noget tekst} f.eks.
  private val colorRegex = """\\([a-zA-Z]+)\{([^}]*)\}""".r

  def render(env: ShellEnv, config: SkaldConfig): List[Span] = {

    val gitBranch = GitStatus.fromPath(env.cwd) match {
      case GitStatus.Branch(name)   => config.gitFormat.replace("%s", name)
      case GitStatus.NotARepository => ""
    }

    val currentDirStr = formatDir(env.cwd.toString, config.dirDepth)
    
    val basePrompt = config.promptTemplate
      .replace("%w", currentDirStr)
      .replace("%b", gitBranch)

    parseSegments(basePrompt + " ")
  }

  private def parseSegments(input: String): List[Span] = {
    val matches = colorRegex.findAllMatchIn(input).toList

    @tailrec
    def aux(remaining: List[scala.util.matching.Regex.Match], lastIdx: Int, acc: List[Span]): List[Span] = remaining match {
      case Nil =>
        if (lastIdx < input.length) {
          (Span(input.substring(lastIdx), Style()) :: acc).reverse
        } else acc.reverse
        
      case m :: tail =>
        val textBefore = if (m.start > lastIdx) {
          Some(Span(input.substring(lastIdx, m.start), Style()))
        } else None

        val style = Color.fromString(m.group(1))
          .map(c => Style(foreground = c.ansiCode))
          .getOrElse(Style())
        val matchedSegment = Span(m.group(2), style)

        val nextAcc = textBefore match {
          case Some(txt) => matchedSegment :: txt :: acc
          case None      => matchedSegment :: acc
        }

        aux(tail, m.end, nextAcc)
    }
    aux(matches, 0, Nil)
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
