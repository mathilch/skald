package skald

sealed trait ExecutionResult
object ExecutionResult {
  case class Success(stdout: String) extends ExecutionResult
  case class Failure(stderr: String, exitCode: Int) extends ExecutionResult
}
