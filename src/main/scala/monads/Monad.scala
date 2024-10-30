package monads

trait Monad[F[_]]:
  def unit[A](a: => A): F[A]

  extension [A](m: F[A])
    def flatMap[B](f: A => F[B]): F[B]

    def map[B](f: A => B): F[B] =
      m.flatMap(a => unit(f(a)))
