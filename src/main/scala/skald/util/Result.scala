package skald


// Lidt mere idiomatisk navngivning end Either med left og right
enum Result[+E, +A]:
  case Fail(error: E)
  case Success(value: A)

  def map[B](f: A => B): Result[E, B] = this match
    case Fail(e)    => Fail(e)
    case Success(a) => Success(f(a))

  def flatMap[E2 >: E, B](f: A => Result[E2, B]): Result[E2, B] = this match
    case Fail(e)    => Fail(e)
    case Success(a) => f(a)

extension [E, A](list: List[Result[E, A]])
  def sequence: Result[E, List[A]] =
    list.foldRight[Result[E, List[A]]](Result.Success(Nil)) { (res, acc) =>
      for {
        x <- res
        xs <- acc
      } yield x :: xs
    } 
