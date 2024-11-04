package core
import cats.implicits.{given}
import cats.data.StateT
import Engine.*
import Behavior.*
import scala.reflect.ClassTag
import scala.reflect.TypeTest
import cats.effect.IO

object GameObject:
  opaque type GameObject = GameObjectImpl
  private case class GameObjectImpl(
      val id: String,
      var behaviors: List[Behavior]
  )

  def apply(id: String, behaviors: List[Behavior]): GameObject =
    GameObjectImpl(id, behaviors)

  extension (go: GameObject)

    def id: String = id

    def behaviors: List[Behavior] = go.behaviors

    def typedBehaviors[T <: Behavior](using TypeTest[Behavior, T]): List[T] =
      go.behaviors.collect({ case b: T => b })

    def onInit(): StateT[IO, Engine, Unit] =
      go.behaviors.foldLeft(StateT.empty[IO, Engine, Unit])((s, b) =>
        s.flatMap(_ => b.onInit(go.id))
      )

    def onEarlyUpdate(): StateT[IO, Engine, Unit] =
      go.behaviors.foldLeft(StateT.empty[IO, Engine, Unit])((s, b) =>
        s.flatMap(_ => b.onEarlyUpdate(go.id))
      )

    def onUpdate(): StateT[IO, Engine, Unit] =
      go.behaviors.foldLeft(StateT.empty[IO, Engine, Unit])((s, b) =>
        s.flatMap(_ => b.onUpdate(go.id))
      )

    def onLateUpdate(): StateT[IO, Engine, Unit] =
      go.behaviors.foldLeft(StateT.empty[IO, Engine, Unit])((s, b) =>
        s.flatMap(_ => b.onLateUpdate(go.id))
      )

    def onDeinit(): StateT[IO, Engine, Unit] =
      go.behaviors.foldLeft(StateT.empty[IO, Engine, Unit])((s, b) =>
        s.flatMap(_ => b.onInit(go.id))
      )

    def updateBehaviors[T <: Behavior](
        f: T => T
    )(using TypeTest[Behavior, T]): GameObject =
      go.copy(behaviors = go.behaviors.map(_ match {
        case b: T => f(b)
        case b    => b
      }))
