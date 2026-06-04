package skald.terminal.grid

import scala.annotation.tailrec

case class Span(text: String, style: Style)
case class Style(foreground: String = "\u001b[37m", bold: Boolean = false)
case class Cell(char: Char, style: Style)

case class Grid(width: Int, height: Int) {
  private val buffer: Array[Cell] = Array.fill(width * height)(Cell(' ', Style()))

  def apply(x: Int, y: Int): Cell = buffer(y * width + x)

  def set(x: Int, y: Int, cell: Cell): Unit = {
    if (x >= 0 && x < width && y >= 0 && y < height) {
      buffer(y * width + x) = cell
    }
  }

  def get(x: Int, y: Int): Cell = {
    if (x >= 0 && x < width && y >= 0 && y < height) {
      buffer(y * width + x)
    } else Cell(' ', Style())
  }

  def putString(x: Int, y: Int, str: String, style: Style): Unit = {
    str.zipWithIndex.foreach { case (char, i) =>
      set(x + i, y, Cell(char, style))
    }
  }

  
  def fill(segments: List[Span], startX: Int = 0): Int = {

    @tailrec
    def aux(segs: List[Span], x: Int): Int = segs match {
      case Nil => x
      case seg :: tail =>
        putString(x, 0, seg.text, seg.style)
        aux(tail, x + seg.text.length)
    }
    aux(segments, startX)
  }


  def toAnsi: String = {
    val sb = new StringBuilder()
    var currentStyle: Option[Style] = None 

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val cell = get(x, y)
        if (currentStyle != Some(cell.style)) {
          sb.append("\u001b[0m").append(cell.style.foreground)
          if (cell.style.bold) sb.append("\u001b[1m")
          currentStyle = Some(cell.style)
        }
        sb.append(cell.char)
      }

      if (y < height - 1) {
        sb.append("\n")
        currentStyle = None
      }
    }
    sb.append("\u001b[0m").toString
  }
}
