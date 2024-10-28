package core

import Engine.*
import Behavior.*
import scala.reflect.ClassTag
import scala.reflect.TypeTest

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

    def onUpdate(engine: Engine): Engine =
      go.behaviors.foldLeft(engine)((e, b) => b.onUpdate(e, go.id))

    def updateBehaviors[T <: Behavior](
        f: T => T
    )(using TypeTest[Behavior, T]): GameObject =
      go.copy(behaviors = go.behaviors.map(_ match {
        case b: T => f(b)
        case b    => b
      }))
