import cats.implicits.{given}
import cats.data.StateT
import core.Engine
import core.Engine.*
import core.GameObject
import core.GameObject.*
import core.Behavior.*
import cats.effect.IO

case class ValueBehavior(var value: Int) extends Behavior:
  override def onUpdate(selfId: String): StateT[IO, Engine, Unit] =
    for _ <- Engine.updateBehaviors[ValueBehavior](selfId)(
        _.copy(value = value + 1)
      )
    yield ()

case class PrintValueBehavior() extends Behavior:
  override def onUpdate(selfId: String): StateT[IO, Engine, Unit] =
    for go <- Engine.findGameObject(selfId)
    yield go.foreach(
      _.typedBehaviors[ValueBehavior].foreach(b => println(b.value))
    )

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
    .unsafeRunSync()
