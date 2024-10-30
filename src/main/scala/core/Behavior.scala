package core

import monads.State.*
import monads.IO.*
import Engine.*

object Behavior:
  trait Behavior:
    def onUpdate(selfId: String): State[IO, Engine, Unit]
