package skald

enum Icon(val code: String):
  case Folder extends Icon("\uf07c")  //  (Mappe)
  case File   extends Icon("\uf15b")  //  (Dokument)
  case Git    extends Icon("\uf126")  //  (Git branch)
  case Arrow  extends Icon("\uf061")  //  (Pil)
  case Shell  extends Icon("\uf120")  //  (Terminal prompt)
  case Check  extends Icon("\uf00c")  //  (Succes markering)
  case Cross  extends Icon("\uf00d")  //  (Fejl markering)
  case Json   extends Icon("\ueb0f")  //  (Json)
  case Scala  extends Icon("\ue737")  //  (Scala)

extension (icon: Icon)
  def withColor(c: Color): String = s"${c.ansiCode}${icon.code}${Color.Reset.ansiCode}"
