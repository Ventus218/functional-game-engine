import core.Engine
import core.Engine.*
import core.GameObject
import core.GameObject.*
import core.Behavior.*

case class ValueBehavior(var value: Int) extends Behavior:
  override def onUpdate(engine: Engine, selfId: String): Engine =
    engine.updateBehaviors[ValueBehavior](selfId)(
      _.copy(value = value + 1)
    )

case class PrintValueBehavior() extends Behavior:
  override def onUpdate(engine: Engine, selfId: String): Engine =
    engine
      .findGameObject(selfId)
      .map(_.typedBehaviors[ValueBehavior].foreach(b => println(b.value)))
    engine

object Main extends App:
  Engine(fpsLimit = 60)
    .scheduleGameObjectCreation(
      GameObject(
        id = "1",
        behaviors = List(ValueBehavior(0), PrintValueBehavior())
      )
    )
    .run()
