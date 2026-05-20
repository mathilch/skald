package skald 

sealed trait JobStatus {
  def displayName: String
}

case object Running extends JobStatus {
  val displayName = "Running"
}

case object Done extends JobStatus {
  val displayName = "Done"
}
