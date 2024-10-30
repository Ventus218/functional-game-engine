package core

import monads.IO.{*, given}
import monads.State.*
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

  // @scala.annotation.tailrec
  def execution(): State[IO, Engine, Unit] =
    for
      startFrameTime <- currentTimeMillis()
      _ <- createGameObjects()
      _ <- deleteGameObjects()
      _ <- executeOnUpdate()
      endComputationTime <- currentTimeMillis()
      computationTime = endComputationTime - startFrameTime
      _ <- sleepIfNecessary(computationTime)
      endFrameTime <- currentTimeMillis()
      _ <- updateDeltaTime(startFrameTime, endFrameTime)
      engine <- getState()
      _ <-
        if engine.shouldStop then sameState()
        else execution()
    yield ()

  def updateGameObject(
      id: String
  )(f: GameObject => GameObject): State[IO, Engine, Unit] =
    State(e =>
      IO((e.copy(gameObjects = e.gameObjects.updatedWith(id)(_.map(f))), ()))
    )

  def findGameObject(id: String): State[IO, Engine, Option[GameObject]] =
    State(e => IO((e, e.gameObjects.get(id))))

  def updateBehaviors[T <: Behavior](id: String)(f: T => T)(using
      TypeTest[Behavior, T]
  ): State[IO, Engine, Unit] =
    updateGameObject(id)(go => go.updateBehaviors(f))

  def scheduleGameObjectCreation(go: GameObject): State[IO, Engine, Unit] =
    State(e =>
      IO(
        (
          e.copy(gameObjectsToCreate = e.gameObjectsToCreate + (go.id -> go)),
          ()
        )
      )
    )

  def createGameObjects(): State[IO, Engine, Unit] =
    State(e =>
      IO(
        (
          e.copy(
            gameObjects = e.gameObjects ++ e.gameObjectsToCreate,
            gameObjectsToCreate = Map()
          ),
          ()
        )
      )
    )

  def scheduleGameObjectDeletion(id: String): State[IO, Engine, Unit] =
    for
      go <- findGameObject(id)
      _ <- go match
        case Some(go) =>
          State((e: Engine) =>
            IO(
              (e.copy(gameObjectsToDelete = e.gameObjectsToDelete + go.id), ())
            )
          )
        case None => sameState()
    yield ()

  def deleteGameObjects(): State[IO, Engine, Unit] =
    State(e =>
      IO(
        (
          e.copy(
            gameObjects = e.gameObjects -- e.gameObjectsToDelete,
            gameObjectsToDelete = Set()
          ),
          ()
        )
      )
    )

  def executeOnUpdate(): State[IO, Engine, Unit] =
    for
      gameObjects <- gameObjects()
      _ <- gameObjects.values.foldLeft(sameState[IO, Engine]())((s, go) =>
        s.flatMap(_ => go.onUpdate())
      )
    yield ()

  def sleepIfNecessary(computationTime: Long): State[IO, Engine, Unit] =
    State(e =>
      if computationTime < (1000 / e.fpsLimit) then
        for _ <- IO.sleep((1000 / e.fpsLimit) - computationTime)
        yield (e, ())
      else IO((e, ()))
    )

  def updateDeltaTime(
      startFrameTime: Long,
      endFrameTime: Long
  ): State[IO, Engine, Unit] =
    State(e =>
      IO((e.copy(deltaTimeMillis = endFrameTime - startFrameTime), ()))
    )

  def currentTimeMillis(): State[IO, Engine, Long] =
    State(e => IO.currentTimeMillis().map((e, _)))

  private def gameObjects(): State[IO, Engine, Map[String, GameObject]] =
    State(e => IO((e, e.gameObjects)))
