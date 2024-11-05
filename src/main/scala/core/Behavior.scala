package core

import cats.data.StateT
import cats.effect.IO
import GameObject.SelfId
import Engine.*

object Behavior:
  trait Behavior[Self <: Behavior[Self]]( /*val enabled: Boolean = true*/ ):

    /** Called only once when the behavior is instatiated. */
    def onInit(using gameObjectId: SelfId): StateT[IO, Engine, Unit] =
      StateT.empty

    /** Called once every frame before onUpdate, only if the behavior is
      * enabled.
      */
    def onEarlyUpdate(using gameObjectId: SelfId): StateT[IO, Engine, Unit] =
      StateT.empty

    /** Called once every frame, only if the behavior is enabled. */
    def onUpdate(using gameObjectId: SelfId): StateT[IO, Engine, Unit] =
      StateT.empty

    /** Called once every frame after onUpdate, only if the behavior is enabled.
      */
    def onLateUpdate(using gameObjectId: SelfId): StateT[IO, Engine, Unit] =
      StateT.empty

    /** Called only once before the engine destroys the object. */
    def onDeinit(using gameObjectId: SelfId): StateT[IO, Engine, Unit] =
      StateT.empty

    /** Called:
      *   - after the behavior is instantiated as enabled
      *   - every time the behavior goes from disabled to enabled.
      */
    // def onEnabled(using gameObjectId: SelfId): StateT[IO, Engine, Unit] = StateT.empty

    /** Called:
      *   - after the behavior is instantiated as disabled
      *   - every time the behavior goes from enabled to disabled.
      */
    // def onDisabled(using gameObjectId: SelfId): StateT[IO, Engine, Unit] = StateT.empty

    import GameObject.*
    import reflect.TypeTest

    def findSelfGameObject()(using
        gameObjectId: SelfId
    ): StateT[IO, Engine, Option[GameObject]] =
      Engine.findGameObject(gameObjectId)

    def findSelfBehaviors()(using
        gameObjectId: SelfId
    )(using TypeTest[Behavior[?], Self]): StateT[IO, Engine, Iterable[Self]] =
      for go <- findSelfGameObject()
      yield (go.map(_.typedBehaviors[Self]).getOrElse(List()))

    def updateSelfGameObject(f: GameObject => GameObject)(using
        gameObjectId: SelfId
    ): StateT[IO, Engine, Unit] =
      Engine.updateGameObject(gameObjectId)(f)

    def updateSelfBehaviors(f: Self => Self)(using
        gameObjectId: SelfId
    )(using TypeTest[Behavior[?], Self]): StateT[IO, Engine, Unit] =
      Engine.updateBehaviors[Self](gameObjectId)(f)
