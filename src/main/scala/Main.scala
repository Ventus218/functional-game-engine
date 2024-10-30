import core.Engine
import core.Engine.*
import core.GameObject
import core.GameObject.*
import core.Behavior.*
import monads.State.*
import monads.IO.*

case class ValueBehavior(var value: Int) extends Behavior:
  override def onUpdate(selfId: String): State[IO, Engine, Unit] =
    for _ <- Engine.updateBehaviors[ValueBehavior](selfId)(
        _.copy(value = value + 1)
      )
    yield ()

case class PrintValueBehavior() extends Behavior:
  override def onUpdate(selfId: String): State[IO, Engine, Unit] =
    for
      go <- Engine.findGameObject(selfId)
      _ <- State[IO, Engine, Unit](s =>
        IO(
          (
            s,
            go.foreach(
              _.typedBehaviors[ValueBehavior].foreach(b => println(b.value))
            )
          )
        )
      )
    yield ()

object Main extends App:
  (for
    _ <-
      Engine
        .scheduleGameObjectCreation(
          GameObject(
            id = "1",
            behaviors = List(ValueBehavior(0), PrintValueBehavior())
          )
        )
    _ <- execution()
  yield ())
    .run(Engine(fpsLimit = 60))
