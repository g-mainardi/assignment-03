package pcd.ass01
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

object BoidsSimulator :
  val FRAMERATE = 50
  private enum Loop:
    case Continue

class BoidsSimulator(protected val model: BoidsModel) {
  import BoidsSimulator.*
  protected var view: Option[BoidsView] = None

  @volatile
  protected var toStart = false
  @volatile
  protected var toResume = false

  private var startingTime = 0L
  protected var framerate = 0
  private var t0 = 0L

  def attachView(view: BoidsView): Unit = this.view = Some(view)

//  def clear(): Unit = ()
//  def init(): Unit = ()

  protected def updateView(): Unit =
    view foreach : v =>
      v.update()
      v updateFrameRate framerate
      val dtElapsed = System.currentTimeMillis - t0
      val frameratePeriod = 1000 / BoidsSimulator.FRAMERATE
      t0 = System.currentTimeMillis
      if dtElapsed < frameratePeriod
      then
        try Thread sleep (frameratePeriod - dtElapsed)
        catch
          case ignore: Exception => ()
        framerate = BoidsSimulator.FRAMERATE
      else framerate = (1000 / dtElapsed).toInt

  protected def suspend(): Unit =
    toResume = true
    view foreach(_.enableSuspendResumeButton())

  protected def resume(): Unit =
    toResume = false
    view foreach(_.enableSuspendResumeButton())

  protected def start(): Unit =
    model.generateBoids()
//    init()
    startingTime = System.currentTimeMillis
    t0 = System.currentTimeMillis
    toStart = false
    view foreach (_.enableStartStopButton())

  protected def stop(): Unit =
//    clear()
    model.clearBoids()
    toStart = true

    if model.isSuspended then
      toResume = false
      view foreach (_.resumeAction())

    view foreach : (v: BoidsView) =>
      v.update()
      v.updateFrameRate(0)
      v.enableStartStopButton()

  private val loop: Behavior[Loop] = Behaviors.receive : (ctx, cmd) =>
    cmd match
      case Loop.Continue =>
        if model.isRunning then
          if toStart then start()
          if model.isSuspended then
            if !toResume then suspend()
          else
            if toResume then resume()
            updateBoids()
          updateView()
        else if !toStart then stop()
        ctx.self ! Loop.Continue
        Behaviors.same

  def runSimulation(): Unit =
    toStart = true
    toResume = false
    ActorSystem(loop, "MainLoop") ! Loop.Continue

  private def updateBoids(): Unit =
    val boids = model.boids
    boids foreach(_.updateVelocity(model))
    boids foreach(_.updatePos(model))
}