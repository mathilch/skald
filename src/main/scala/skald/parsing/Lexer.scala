package skald 

object Lexer {

  def tokenizeInput(input: String): List[String] = {
    def aux(str: List[Char], token: List[Char], acc: List[String], currentQuote: Option[Char]): List[String] = {
      str match {
        case Nil => 
          if (token.nonEmpty) (token.reverse.mkString :: acc).reverse
          else acc.reverse
        case '\\' :: c :: tail => 
          if (currentQuote.contains('\'')) aux(c :: tail, '\\' :: token, acc, currentQuote)
          else aux(tail, c :: token, acc, currentQuote)

        case (q @ ('\'' | '\"')) :: tail =>
          currentQuote match {
            case Some(`q`) => aux(tail, token, acc, None)
            case Some(_) => aux(tail, q :: token, acc, currentQuote)
            case None => aux(tail, token, acc, Some(`q`))
          }
        case ' ' :: tail if currentQuote.isEmpty =>
          if (token.nonEmpty) aux(tail, Nil, token.reverse.mkString :: acc, currentQuote)
          else aux(tail, Nil, acc, currentQuote)

        case c :: tail => aux(tail, c :: token, acc, currentQuote)
      }
    }
    aux(input.toList, Nil, Nil, None)
  }
}
