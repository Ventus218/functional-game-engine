package monads

object State:
  case class State[F[_]: Monad, S, O](run: (S) => F[(S, O)])

  given [F[_]: Monad, S]: Monad[[A] =>> State[F, S, A]] with

    def unit[A](a: => A): State[F, S, A] =
      val monadF = summon[Monad[F]]
      State(s => monadF.unit((s, a)))

    extension [A](m: State[F, S, A])
      def flatMap[B](f: A => State[F, S, B]): State[F, S, B] =
        val monadF = summon[Monad[F]]
        State(s =>
          val res = m.run(s)
          res.flatMap((newState, output) => f(output).run(newState))
        )

  def sameState[F[_]: Monad, S](): State[F, S, Unit] =
    val monadF = summon[Monad[F]]
    State(s => monadF.unit((s, ())))

  def getState[F[_]: Monad, S](): State[F, S, S] =
    val monadF = summon[Monad[F]]
    State(s => monadF.unit((s, s)))

object StateTest extends App:
  import IO.{*, given}
  import monads.State.sameState
  val getState = State.State((s: Int) => IO((s, s.toString())))

  for
    s <- getState
    s2 <- sameState()
  yield ()
