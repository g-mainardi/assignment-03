package pcd.ass01
import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object BoidsSimulator :
  private val FRAMERATE = 50
  private enum Command:
    case UpdateBoidsPos
    case UpdateBoidsVel
  private enum Loop:
    case Continue
    case Update

class BoidsSimulator(private val model: BoidsModel) {
  import BoidsSimulator.{Command, Loop}

  private var mainLoop: Option[ActorSystem[Loop]] = None

  private var view: Option[BoidsView] = None

  private var toStart = false
  private var toResume = false

  private var startingTime = 0L
  private var framerate = 0
  private var t0 = 0L

  def attachView(view: BoidsView): Unit = this.view = Some(view)

//  def clear(): Unit = ()
//  def init(): Unit = ()

  private def updateView(): Unit =
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

  private def suspend(): Unit =
    toResume = true
    view foreach(_.enableSuspendResumeButton())

  private def resume(): Unit =
    toResume = false
    view foreach(_.enableSuspendResumeButton())

  private def start(): Unit =
    model.generateBoids()
//    init()
    startingTime = System.currentTimeMillis
    t0 = System.currentTimeMillis
    toStart = false
    view foreach (_.enableStartStopButton())

  private def stop(): Unit =
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

  private val loop: Behavior[Loop] =
    import Loop.*
    Behaviors.receive : (ctx, cmd) =>
      cmd match
      case Update =>
        updateView()
        ctx.self ! Continue
        Behaviors.same
      case Continue =>
        if model.isRunning then
          if toStart then start()
          if model.isSuspended then
            if !toResume then suspend()
            ctx.self ! Continue
            Behaviors.same
          else
            if toResume then resume()
            (ctx spawnAnonymous updateBoids) ! Command.UpdateBoidsVel
            Behaviors.same
        else
          if !toStart then stop()
          ctx.self ! Continue
          Behaviors.same

  def runSimulation(): Unit =
    toStart = true
    toResume = false
    mainLoop = Some(ActorSystem(loop, "MainLoop"))
    import SimulationParameter.*
    view = Some(BoidsView(model, SCREEN_WIDTH, SCREEN_HEIGHT))
    mainLoop foreach{_ ! Loop.Continue}

  import Command.*
  private val updateBoids: Behavior[BoidsSimulator.Command] = Behaviors.receive: (ctx, cmd) =>
    cmd match
    case UpdateBoidsVel =>
      model.boids foreach {_.updateVelocity(model)}
      ctx.self ! UpdateBoidsPos
      Behaviors.same
    case UpdateBoidsPos =>
      model.boids foreach {_.updatePos(model)}
      mainLoop foreach{_ ! Loop.Update}
      Behaviors.stopped
}