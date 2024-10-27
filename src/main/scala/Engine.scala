import monads.IO.*
import scala.annotation.tailrec
object Engine:

  final case class Engine(
      gameObjects: Map[String, GameObject],
      fpsLimit: Int,
      deltaTimeMillis: Long = 0,
      shouldStop: Boolean = false
  )
  final case class GameObject(val id: String, var behaviors: List[Behavior])
  trait Behavior:
    def apply(engine: Engine, selfId: String): Engine = engine

  extension (e: Engine)
    @tailrec
    def run(): Engine =
      var computeFrame = for
        startFrameTime <- currentTimeMillis()
        newEngine = e.gameObjects.values.foldLeft(e)((e, gameObject) =>
          gameObject.behaviors.foldLeft(e)((e, behavior) =>
            behavior(e, gameObject.id)
          )
        )
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

  extension (go: GameObject)
    def updateBehavior(index: Int)(f: Behavior => Behavior): GameObject =
      go.copy(behaviors =
        go.behaviors.zipWithIndex.map((b, i) => if i == index then f(b) else b)
      )
