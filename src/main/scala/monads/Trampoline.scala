package monads

import scala.annotation.tailrec

object Trampoline:

  enum Trampoline[A]:
    case Done(value: A)
    case More(work: () => Trampoline[A])

  import Trampoline.*

  def done[A](value: A): Trampoline[A] =
    Done(value)
  def more[A](work: () => Trampoline[A]): Trampoline[A] =
    More(work)

  given Monad[Trampoline] with
    def unit[A](a: => A): Trampoline[A] =
      More(() => Done(a))

    extension [A](m: Trampoline[A])
      def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = m match
        case Done(value) => f(value)
        case More(work)  => More(() => work().flatMap(f))

      override def map[B](f: A => B): Trampoline[B] = m.flatMap(a => Done(f(a)))

  extension [A](t: Trampoline[A])
    @tailrec
    def evaluate(): A =
      t match
        case Done(value) => value
        case More(work)  => work().evaluate()

object TestTrampoline extends App:
  import Trampoline.{*, given}

  extension [A](l: List[A])
    def myZipIndex(i: Int = 0): List[(A, Int)] =
      l.headOption match
        case Some(head) => (head, i) :: l.tail.myZipIndex(i + 1)
        case None        => List()

    def myZipIndexT(i: Int = 0): Trampoline[List[(A, Int)]] =
      l.headOption match
        case Some(head) =>
          more(() => l.tail.myZipIndexT(i + 1).map(l => (head, i) :: l))
        case None => done(List())

  // println(List.fill(5000)("s").myZipIndex())
  println(List.fill(20000)("s").myZipIndexT().evaluate())
