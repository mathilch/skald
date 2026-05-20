package skald 

sealed trait Completion

case object NoMatch extends Completion 
case class SingleMatch(completed: String) extends Completion
case class MultipleMatches(lcp: String, options: List[String]) extends Completion
