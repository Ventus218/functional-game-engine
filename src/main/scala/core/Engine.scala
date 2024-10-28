package core

import monads.IO.*
import GameObject.*
import Behavior.*
import scala.reflect.TypeTest

object Engine:
  opaque type Engine = EngineImpl
  private case class EngineImpl(
      gameObjects: Map[String, GameObject] = Map(),
      gameObjectsToCreate: Map[String, GameObject] = Map(),
      gameObjectsToDelete: Set[String] = Set(),
      fpsLimit: Int,
      deltaTimeMillis: Long = 0,
      shouldStop: Boolean = false
  )

  def apply(fpsLimit: Int): Engine =
    EngineImpl(fpsLimit = fpsLimit)

  extension (e: Engine)
    @scala.annotation.tailrec
    def run(): Engine =
      // TODO: fix e1, e2, e3, e4, .. that's ugly
      var computeFrame = for
        startFrameTime <- currentTimeMillis()
        e1 = e.copy(
          gameObjects = e.gameObjects ++ e.gameObjectsToCreate,
          gameObjectsToCreate = Map()
        )
        e2 = e1.copy(
          gameObjects = e1.gameObjects -- e1.gameObjectsToDelete,
          gameObjectsToDelete = Set()
        )
        e3 = e2.gameObjects.values.foldLeft(e2)((e, go) => go.onUpdate(e))
        endComputationTime <- currentTimeMillis()
        computationTime = endComputationTime - startFrameTime
        _ <-
          if computationTime < (1000 / e3.fpsLimit) then
            sleep((1000 / e3.fpsLimit) - computationTime)
          else nop()
        endFrameTime <- currentTimeMillis()
      yield (e3.copy(deltaTimeMillis = endFrameTime - startFrameTime))

      var e4 = computeFrame.run()
      e4.shouldStop match
        case true  => e4
        case false => e4.run()

    def updateGameObject(id: String)(f: GameObject => GameObject): Engine =
      e.copy(gameObjects = e.gameObjects.updatedWith(id)(_.map(f)))

    def findGameObject(id: String): Option[GameObject] =
      e.gameObjects.get(id)

    def updateBehaviors[T <: Behavior](id: String)(f: T => T)(using
        TypeTest[Behavior, T]
    ): Engine =
      e.updateGameObject(id)(go => go.updateBehaviors(f))

    def scheduleGameObjectCreation(go: GameObject): Engine =
      e.copy(gameObjectsToCreate = e.gameObjectsToCreate + (go.id -> go))

    def scheduleGameObjectDeletion(id: String): Engine =
      e.findGameObject(id) match
        case Some(go) =>
          e.copy(gameObjectsToDelete = e.gameObjectsToDelete + go.id)
        case None => e
