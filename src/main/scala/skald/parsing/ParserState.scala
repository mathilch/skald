package skald

import Result._

case class ParserState(tokens: List[String]) {
  def peek: Option[String] = tokens.headOption 

  def next: ParserState = ParserState(tokens.tail)

  def consume: (String, ParserState) = (tokens.head, ParserState(tokens.tail))

  def isEmpty: Boolean = tokens.isEmpty

  def expect(expected: String, errorMsg: String): Result[ParseError, Unit] = {
    peek match {
      case Some(t) if t == expected => Success(())
      case Some(t) => Fail(ParseError.InvalidSyntax(s"$errorMsg (found '$t')"))
      case None    => Fail(ParseError.InvalidSyntax(s"$errorMsg (reached EOF)"))
    }
  }
}
