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
      _ <- executeOnEarlyUpdate()
      _ <- executeOnUpdate()
      _ <- executeOnLateUpdate()
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

  def updateBehaviors[T <: Behavior[T]](id: String)(f: T => T)(using
      TypeTest[Behavior[?], T]
  ): StateT[IO, Engine, Unit] =
    updateGameObject(id)(go => go.updateBehaviors(f))

  def scheduleGameObjectCreation(go: GameObject): StateT[IO, Engine, Unit] =
    StateT.modify(e =>
      e.copy(gameObjectsToCreate = e.gameObjectsToCreate + (go.id -> go))
    )

  private def createGameObjects(): StateT[IO, Engine, Unit] =
    for
      _ <- StateT.modify[IO, Engine](e =>
        e.copy(gameObjects = e.gameObjects ++ e.gameObjectsToCreate)
      )
      createdGameObjects <- StateT.inspect[IO, Engine, Iterable[GameObject]](
        _.gameObjectsToCreate.values
      )
      _ <- createdGameObjects.foldLeft(StateT.empty[IO, Engine, Unit])(
        (s, go) => s.flatMap(_ => go.onInit())
      )
      _ <- StateT.modify[IO, Engine](_.copy(gameObjectsToCreate = Map()))
    yield ()

  def scheduleGameObjectDeletion(id: String): StateT[IO, Engine, Unit] =
    StateT.modify(e => e.copy(gameObjectsToDelete = e.gameObjectsToDelete + id))

  private def deleteGameObjects(): StateT[IO, Engine, Unit] =
    for
      gameObjectsToDelete <- StateT.inspect[IO, Engine, Iterable[GameObject]](
        e =>
          e.gameObjectsToDelete.flatMap(id =>
            e.gameObjects.get(id).map(Set(_)).getOrElse(Set())
          )
      )
      _ <- gameObjectsToDelete.foldLeft(StateT.empty[IO, Engine, Unit])(
        (s, go) => s.flatMap(_ => go.onDeinit())
      )
      _ <- StateT.modify[IO, Engine](e =>
        e.copy(
          gameObjects = e.gameObjects -- e.gameObjectsToDelete,
          gameObjectsToDelete = Set()
        )
      )
    yield ()

  private def executeOnEarlyUpdate(): StateT[IO, Engine, Unit] =
    for
      gameObjects <- gameObjects()
      _ <- gameObjects.values.foldLeft(StateT.empty[IO, Engine, Unit])(
        (s, go) => s.flatMap(_ => go.onEarlyUpdate())
      )
    yield ()

  private def executeOnUpdate(): StateT[IO, Engine, Unit] =
    for
      gameObjects <- gameObjects()
      _ <- gameObjects.values.foldLeft(StateT.empty[IO, Engine, Unit])(
        (s, go) => s.flatMap(_ => go.onUpdate())
      )
    yield ()

  private def executeOnLateUpdate(): StateT[IO, Engine, Unit] =
    for
      gameObjects <- gameObjects()
      _ <- gameObjects.values.foldLeft(StateT.empty[IO, Engine, Unit])(
        (s, go) => s.flatMap(_ => go.onLateUpdate())
      )
    yield ()

  private def sleepIfNecessary(
      computationTime: Long
  ): StateT[IO, Engine, Unit] =
    for
      fpsLimit <- StateT.inspect[IO, Engine, Int](_.fpsLimit)
      _ <-
        if computationTime < (1000 / fpsLimit) then
          StateT.lift(IO(Thread.sleep((1000 / fpsLimit) - computationTime)))
        else StateT.empty[IO, Engine, Unit]
    yield ()

  def deltaTimeMillis(): StateT[IO, Engine, Long] =
    StateT.inspect(_.deltaTimeMillis)

  def deltaTimeSeconds(): StateT[IO, Engine, Double] =
    StateT.inspect(_.deltaTimeMillis / 1000d)

  private def updateDeltaTime(
      startFrameTime: Long,
      endFrameTime: Long
  ): StateT[IO, Engine, Unit] =
    StateT.modify(e => e.copy(deltaTimeMillis = endFrameTime - startFrameTime))

  private def currentTimeMillis(): StateT[IO, Engine, Long] =
    StateT.lift(IO(System.currentTimeMillis()))

  private def gameObjects(): StateT[IO, Engine, Map[String, GameObject]] =
    StateT.inspect(_.gameObjects)
