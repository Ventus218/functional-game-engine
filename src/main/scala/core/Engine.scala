package core

import cats.implicits.{given}
import cats.data.StateT
import cats.effect.IO
import GameObject.*
import Behavior.*
import scala.reflect.TypeTest
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

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
  def execution(): StateT[IO, Engine, Unit] =
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
      engine <- StateT.get
      _ <-
        if engine.shouldStop then StateT.empty[IO, Engine, Unit]
        else execution()
    yield ()

  def updateGameObject(
      id: String
  )(f: GameObject => GameObject): StateT[IO, Engine, Unit] =
    StateT.modify(e =>
      e.copy(gameObjects = e.gameObjects.updatedWith(id)(_.map(f)))
    )

  def findGameObject(id: String): StateT[IO, Engine, Option[GameObject]] =
    StateT.inspect(e => e.gameObjects.get(id))

  def updateBehaviors[T <: Behavior](id: String)(f: T => T)(using
      TypeTest[Behavior, T]
  ): StateT[IO, Engine, Unit] =
    updateGameObject(id)(go => go.updateBehaviors(f))

  def scheduleGameObjectCreation(go: GameObject): StateT[IO, Engine, Unit] =
    StateT.modify(e =>
      e.copy(gameObjectsToCreate = e.gameObjectsToCreate + (go.id -> go))
    )

  def createGameObjects(): StateT[IO, Engine, Unit] =
    StateT.modify(e =>
      e.copy(
        gameObjects = e.gameObjects ++ e.gameObjectsToCreate,
        gameObjectsToCreate = Map()
      )
    )

  def scheduleGameObjectDeletion(id: String): StateT[IO, Engine, Unit] =
    StateT.modify(e => e.copy(gameObjectsToDelete = e.gameObjectsToDelete + id))

  def deleteGameObjects(): StateT[IO, Engine, Unit] =
    StateT.modify(e =>
      e.copy(
        gameObjects = e.gameObjects -- e.gameObjectsToDelete,
        gameObjectsToDelete = Set()
      )
    )

  def executeOnUpdate(): StateT[IO, Engine, Unit] =
    for
      gameObjects <- gameObjects()
      _ <- gameObjects.values.foldLeft(StateT.empty[IO, Engine, Unit])(
        (s, go) => s.flatMap(_ => go.onUpdate())
      )
    yield ()

  def sleepIfNecessary(computationTime: Long): StateT[IO, Engine, Unit] =
    StateT(e =>
      if computationTime < (1000 / e.fpsLimit) then
        for _ <- IO(Thread.sleep((1000 / e.fpsLimit) - computationTime))
        yield (e, ())
      else IO((e, ()))
    )

  def updateDeltaTime(
      startFrameTime: Long,
      endFrameTime: Long
  ): StateT[IO, Engine, Unit] =
    StateT.modify(e => e.copy(deltaTimeMillis = endFrameTime - startFrameTime))

  def currentTimeMillis(): StateT[IO, Engine, Long] =
    StateT.lift(IO(System.currentTimeMillis()))

  private def gameObjects(): StateT[IO, Engine, Map[String, GameObject]] =
    StateT.inspect(_.gameObjects)
