package core

import cats.data.StateT
import cats.effect.IO
import Engine.*

object Behavior:
  trait Behavior:
    def onUpdate(selfId: String): StateT[IO, Engine, Unit]
