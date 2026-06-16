package skald

case class ShellState(
  current: ShellEnv,
  history: List[ShellEnv] = Nil
) {

  def update(newEnv: ShellEnv): ShellState = {
    if (newEnv != current) ShellState(newEnv, current :: history)
    else this
  }

  def undo: ShellState = history match {
    case prev :: tail => ShellState(prev, tail)
    case Nil          => this
  }

}
