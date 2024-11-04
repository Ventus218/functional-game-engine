package core

import cats.data.StateT
import cats.effect.IO
import Engine.*

object Behavior:
  trait Behavior(val enabled: Boolean = true):

    /** Called only once when the behavior is instatiated. */
    def onInit(selfId: String): StateT[IO, Engine, Unit] = StateT.empty

    /** Called once every frame before onUpdate, only if the behavior is
      * enabled.
      */
    def onEarlyUpdate(selfId: String): StateT[IO, Engine, Unit] = StateT.empty

    /** Called once every frame, only if the behavior is enabled. */
    def onUpdate(selfId: String): StateT[IO, Engine, Unit] = StateT.empty

    /** Called once every frame after onUpdate, only if the behavior is enabled.
      */
    def onLateUpdate(selfId: String): StateT[IO, Engine, Unit] = StateT.empty

    /** Called only once before the engine destroys the object. */
    def onDeinit(selfId: String): StateT[IO, Engine, Unit] = StateT.empty

