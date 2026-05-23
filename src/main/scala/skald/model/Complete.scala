package skald 

enum Completion:
  case NoMatch
  case SingleMatch(completed: String)
  case MultipleMatches(lcp: String, options: List[String])
