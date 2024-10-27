package monads

object IO:
  case class IO[A](run: () => A)

  extension [A](io: IO[A])
    def map[B](f: A => B): IO[B] =
      IO(() => f(io.run()))

    def flatMap[B](f: A => IO[B]): IO[B] =
      f(io.run())

  def nop(): IO[Unit] =
    IO(() => ())

  def sleep(millis: Long): IO[Unit] =
    IO(() => Thread.sleep(millis))

  def currentTimeMillis(): IO[Long] =
    IO(() => System.currentTimeMillis())
