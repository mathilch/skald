package skald 

enum JobStatus(val displayName: String):
  case Running extends JobStatus("Running")
  case Done    extends JobStatus("Done")
