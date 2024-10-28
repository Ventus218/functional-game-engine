package core

import Engine.*

object Behavior:
  trait Behavior:
    def onUpdate(engine: Engine, selfId: String): Engine
