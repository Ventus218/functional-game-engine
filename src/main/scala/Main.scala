import cats.implicits.{given}
import cats.data.StateT
import core.Engine
import core.Engine.*
import core.GameObject
import core.GameObject.*
import core.Behavior.*
import cats.effect.IO

case class Vector2D(val x: Double, val y: Double):
  infix def +(other: Vector2D): Vector2D =
    Vector2D(x + other.x, y + other.y)
  infix def *(scalar: Double): Vector2D =
    Vector2D(x * scalar, y * scalar)

case class Position(val position: Vector2D) extends Behavior[Position]:
  override def onUpdate(selfId: String): StateT[IO, Engine, Unit] =
    StateT.lift(IO(println(s"x: ${position.x}\ty: ${position.y}")))

case class Velocity(val velocity: Vector2D) extends Behavior[Velocity]:
  override def onUpdate(selfId: String): StateT[IO, Engine, Unit] =
    for
      dt <- Engine.deltaTimeSeconds()
      _ <- Engine.updateBehaviors[Position](selfId)(p =>
        p.copy(p.position + (velocity * dt))
      )
    yield ()

  def impulse(force: Vector2D): Velocity =
    this.copy(velocity + force)

case class Acceleration(val acceleration: Vector2D)
    extends Behavior[Acceleration]:
  override def onUpdate(selfId: String): StateT[IO, Engine, Unit] =
    for
      dt <- Engine.deltaTimeSeconds()
      _ <- Engine.updateBehaviors[Velocity](selfId)(v =>
        v.copy(v.velocity + (acceleration * dt))
      )
    yield ()

case class Jump(
    debounceFrames: Int = 5,
    private val framesPassedSinceLastJump: Int = 0
) extends Behavior[Jump]:
  override def onUpdate(selfId: String): StateT[IO, Engine, Unit] =
    for
      go <- Engine.findGameObject(selfId)
      shouldJump = (for
        go <- go
        position <- go.typedBehaviors[Position].headOption
      yield (position.position.y <= 0 && framesPassedSinceLastJump > debounceFrames))
        .getOrElse(false)
      _ <-
        if shouldJump then
          for
            _ <-
              Engine.updateBehaviors[Velocity](selfId)(
                _.impulse(Vector2D(0, 30))
              )
            _ <- StateT.lift(IO(println("jump!!")))
            _ <- Engine.updateBehaviors[Jump](selfId)(
              _.copy(framesPassedSinceLastJump = -1) // it will soon become 0
            )
          yield ()
        else StateT.empty[IO, Engine, Unit]
      _ <- Engine.updateBehaviors[Jump](selfId)(
        _.copy(framesPassedSinceLastJump = framesPassedSinceLastJump + 1)
      )
    yield ()

object Main extends App:
  val ballGameObject =
    GameObject(
      id = "ball",
      behaviors = List(
        Acceleration(Vector2D(0, -9.81)),
        Velocity(Vector2D(0, 0)),
        Position(Vector2D(0, 10)),
        Jump()
      )
    )
  (for
    _ <- Engine.scheduleGameObjectCreation(ballGameObject)
    _ <- execution()
  yield ())
    .run(Engine(fpsLimit = 60))
    .unsafeRunSync()
