package skald.terminal.grid

import scala.annotation.tailrec

case class Span(text: String, style: Style)
case class Style(foreground: String = "\u001b[37m", bold: Boolean = false)
case class Cell(char: Char, style: Style)

case class Grid(width: Int, height: Int) {
  private val grid: Array[Cell] = Array.fill(width * height)(Cell(' ', Style()))

  def apply(x: Int, y: Int): Cell = grid(y * width + x)

  def set(index: Int, cell: Cell): Unit = {
    if (index >= 0 && index < grid.length) {
      grid(index) = cell
    }
  }

  def putString(startIndex: Int, str: String, style: Style): Int = {
    str.zipWithIndex.foreach { case (char, i) =>
      set(startIndex + i, Cell(char, style))
    }
    startIndex + str.length 
  }

  // 3. Din fill-metode bliver nu ekstremt simpel
  def fill(segments: List[Span], startPos: Int = 0): Int = {
    segments.foldLeft(startPos) { (currentPos, span) =>
      putString(currentPos, span.text, span.style)
    }
  }


  def toAnsi: String = {
    val sb = new StringBuilder()
    var currentStyle: Option[Style] = None 

    for (i <- grid.indices) {
      val cell = grid(i)
      
      if (currentStyle != Some(cell.style)) {
        sb.append("\u001b[0m").append(cell.style.foreground)
        if (cell.style.bold) sb.append("\u001b[1m")
        currentStyle = Some(cell.style)
      }

      sb.append(cell.char)

      if ((i + 1) % width == 0 && i < grid.length - 1) {
        sb.append("\n")
        currentStyle = None 
      }
    }
    
    sb.append("\u001b[0m").toString
  }
}
