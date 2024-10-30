package core

import Engine.*
import Behavior.*
import scala.reflect.ClassTag
import scala.reflect.TypeTest
import monads.State.*
import monads.IO.{*, given}

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

    def onUpdate(): State[IO, Engine, Unit] =
      go.behaviors.foldLeft(sameState())((s, b) =>
        s.flatMap(_ => b.onUpdate(go.id))
      )

    def updateBehaviors[T <: Behavior](
        f: T => T
    )(using TypeTest[Behavior, T]): GameObject =
      go.copy(behaviors = go.behaviors.map(_ match {
        case b: T => f(b)
        case b    => b
      }))
