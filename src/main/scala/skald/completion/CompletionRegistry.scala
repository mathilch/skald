package skald

object CompletionRegistry {
  private val state = scala.collection.mutable.Map[String, String]()

  def register(path: String, cmd: String) = state.put(cmd, path)
  def unregister(cmd: String) = state.remove(cmd)
  def get(cmd: String) = state.get(cmd)
}
