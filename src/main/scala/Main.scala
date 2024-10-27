import Engine.*

case class ValueBehavior(var value: Int) extends Behavior:
  override def apply(engine: Engine, selfId: String): Engine =
    engine.updateGameObject(selfId)(
      _.updateBehavior(0)(
        _.asInstanceOf[ValueBehavior].copy(value = value + 1)
      )
    )

case class PrintValueBehavior() extends Behavior:
  override def apply(engine: Engine, selfId: String): Engine =
    engine.updateGameObject(selfId)(
      _.updateBehavior(0)(b =>
        // Not updating but just printing
        println(b.asInstanceOf[ValueBehavior].value)
        b
      )
    )

object Main extends App:
  Engine(
    Map(
      "1" ->
        new GameObject(
          "1",
          List(ValueBehavior(0), PrintValueBehavior())
        )
    ),
    fpsLimit = 60
  ).run()
