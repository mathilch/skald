package skald 

case class Trie(
  endOfWord: Boolean = false, 
  children: Map[Char, Trie] = Map.empty
) { 
  def insert(s: String): Trie = 
    if (s.isEmpty) this.copy(endOfWord = true)
    else {
      val c = s.head
      val next = children.getOrElse(c, Trie())
      this.copy(children = children + (c -> next.insert(s.tail)))
    }

  def findPrefix(prefix: String): Option[Trie] =
    if (prefix.isEmpty()) Some(this)
    else children.get(prefix.head).flatMap(_.findPrefix(prefix.tail))


  def complete(prefix: String): List[String] =
    val res = if (endOfWord) List(prefix) else Nil
    val childRes = children.toList.flatMap {
      case (char, trie) => trie.complete(prefix + char)
    }
    res ++ childRes

  def lcpForAll(strs: List[String]): String =
    if (strs.isEmpty) ""
    else {
      val sorted = strs.sorted
      val first = sorted.head
      val last = sorted.last

      first.zip(last).takeWhile(p => p._1 == p._2).map(_._1).mkString
    }

}

object Trie {
  def load(): Trie = {
    val autocompletes = List("echo", "exit")
    val paths = Path.getAllExecutables()

    (autocompletes ++ paths).foldLeft(Trie())((trie, cmd) => trie.insert(cmd))
  }
}
