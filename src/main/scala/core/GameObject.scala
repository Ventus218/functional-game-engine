package core
import cats.implicits.{given}
import cats.data.StateT
import Engine.*
import Behavior.*
import scala.reflect.ClassTag
import scala.reflect.TypeTest
import cats.effect.IO

object GameObject:
  opaque type SelfId = String

  given Conversion[SelfId, String] with
    def apply(x: SelfId): String = x

  opaque type GameObject = GameObjectImpl
  private case class GameObjectImpl(
      val id: String,
      var behaviors: List[Behavior[?]]
  )

  def apply(id: String, behaviors: List[Behavior[?]]): GameObject =
    GameObjectImpl(id, behaviors)

  extension (go: GameObject)

    def id: String = id

    def behaviors: List[Behavior[?]] = go.behaviors

    def typedBehaviors[T <: Behavior[T]](using
        TypeTest[Behavior[?], T]
    ): List[T] =
      go.behaviors.collect({ case b: T => b })

    def onInit(): StateT[IO, Engine, Unit] =
      go.behaviors.foldLeft(StateT.empty[IO, Engine, Unit])((s, b) =>
        s.flatMap(_ => b.onInit(using go.id))
      )

    def onEarlyUpdate(): StateT[IO, Engine, Unit] =
      go.behaviors.foldLeft(StateT.empty[IO, Engine, Unit])((s, b) =>
        s.flatMap(_ => b.onEarlyUpdate(using go.id))
      )

    def onUpdate(): StateT[IO, Engine, Unit] =
      go.behaviors.foldLeft(StateT.empty[IO, Engine, Unit])((s, b) =>
        s.flatMap(_ => b.onUpdate(using go.id))
      )

    def onLateUpdate(): StateT[IO, Engine, Unit] =
      go.behaviors.foldLeft(StateT.empty[IO, Engine, Unit])((s, b) =>
        s.flatMap(_ => b.onLateUpdate(using go.id))
      )

    def onDeinit(): StateT[IO, Engine, Unit] =
      go.behaviors.foldLeft(StateT.empty[IO, Engine, Unit])((s, b) =>
        s.flatMap(_ => b.onInit(using go.id))
      )

    def updateBehaviors[T <: Behavior[T]](
        f: T => T
    )(using TypeTest[Behavior[?], T]): GameObject =
      go.copy(behaviors = go.behaviors.map(_ match {
        case b: T => f(b)
        case b    => b
      }))
