package monads

object IO:
  opaque type IO[A] = IOImpl[A]
  private case class IOImpl[A](run: () => A)

  extension [A](io: IO[A])
    def run(): A =
      io.run()

  given Monad[IO] with
    def unit[A](a: => A): IO[A] = IOImpl(run = () => a)

    extension [A](io: IO[A])
      def flatMap[B](f: A => IO[B]): IO[B] =
        f(io.run())

  object IO: // just for syntax IO.xxx
    def apply[A](a: => A)(using m: Monad[IO]): IO[A] =
      m.unit(a)

    def nop()(using m: Monad[IO]): IO[Unit] =
      m.unit(())

    def sleep(millis: Long)(using m: Monad[IO]): IO[Unit] =
      m.unit(Thread.sleep(millis))

    def currentTimeMillis()(using m: Monad[IO]): IO[Long] =
      m.unit(System.currentTimeMillis())

object TestIO extends App:
  var io = IO.IO(println("before"))
  println("after")
  (for
    _ <- io
    _ <- IO.IO(println("after2"))
  yield ()).run()
