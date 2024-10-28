package core

import monads.IO.*
import GameObject.*
import Behavior.*
import scala.reflect.TypeTest

object Engine:
  opaque type Engine = EngineImpl
  private case class EngineImpl(
      gameObjects: Map[String, GameObject],
      fpsLimit: Int,
      deltaTimeMillis: Long = 0,
      shouldStop: Boolean = false
  )

  def apply(
      fpsLimit: Int, /*TODO: remove objects*/ objects: List[GameObject]
  ): Engine =
    EngineImpl(Map(objects.map(o => (o.id -> o))*), fpsLimit)

  extension (e: Engine)
    @scala.annotation.tailrec
    def run(): Engine =
      var computeFrame = for
        startFrameTime <- currentTimeMillis()
        newEngine = e.gameObjects.values.foldLeft(e)((e, go) => go.onUpdate(e))
        endComputationTime <- currentTimeMillis()
        computationTime = endComputationTime - startFrameTime
        _ <-
          if computationTime < (1000 / e.fpsLimit) then
            sleep((1000 / e.fpsLimit) - computationTime)
          else nop()
        endFrameTime <- currentTimeMillis()
      yield (newEngine.copy(deltaTimeMillis = endFrameTime - startFrameTime))

      var newEngine = computeFrame.run()
      newEngine.shouldStop match
        case true  => newEngine
        case false => newEngine.run()

    def updateGameObject(id: String)(f: GameObject => GameObject): Engine =
      e.copy(gameObjects = e.gameObjects.updatedWith(id)(_.map(f)))

    def findGameObject(id: String): Option[GameObject] =
      e.gameObjects.get(id)

    def updateBehaviors[T <: Behavior](id: String)(f: T => T)(using
        TypeTest[Behavior, T]
    ): Engine =
      e.updateGameObject(id)(go => go.updateBehaviors(f))
