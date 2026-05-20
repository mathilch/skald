package skald 

object Util {

  def sequence[A](maybeList: List[Option[A]]): Option[List[A]] =
    maybeList.foldLeft(Option(List.empty[A])) { (accOpt, elemOpt) =>
      for {
        acc <- accOpt
        elem <- elemOpt
      } yield elem :: acc
    }.map(_.reverse)

}
