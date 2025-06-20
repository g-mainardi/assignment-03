package pcd.ass01
import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}

object BoidsSimulator :
  private val FRAMERATE = 50
  private enum Command:
    case UpdateBoidsPos
    case UpdateBoidsVel
  enum Loop:
    case Start(nBoids: String)
    case Stop
    case Suspend
    case Resume
    case UpdateBoids
    case UpdateView
    case ChangeAttribute(a: Attribute, value: Double)

class BoidsSimulator(private val model: BoidsModel) {
  import BoidsSimulator.{Command, Loop}

  private var mainLoop: Option[ActorSystem[Loop]] = None

  private var view: Option[BoidsView] = None

  private var startingTime = 0L
  private var framerate = 0
  private var t0 = 0L

//  def clear(): Unit = ()
//  def init(): Unit = ()

  private val updateView: Behavior[Loop] = Behaviors.setup: ctx =>
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
    ctx.self ! Loop.UpdateBoids
    running

  private def suspend(): Unit =
    view foreach(_.suspendAction())
    view foreach(_.enableSuspendResumeButton())

  private def resume(): Unit =
    view foreach(_.resumeAction())
    view foreach(_.enableSuspendResumeButton())

  private def start(): Unit =
    view foreach {_.startAction()}
    model.generateBoids()
//    init()
    startingTime = System.currentTimeMillis
    t0 = System.currentTimeMillis
    view foreach (_.enableStartStopButton())

  private val validateNBoids: Int => Unit =
    case n if n > 0  => model setBoidsNumber n
    case _ => throw new IllegalArgumentException

  import Loop.*
  private val handleAttributeChanging: PartialFunction[Loop, Behavior[Loop]] =
    case ChangeAttribute(attr, value) => model setWeight(attr, value); Behaviors.same
    
  private val notRunning: Behavior[Loop] = Behaviors.receivePartial:
    case (ctx, cmd: ChangeAttribute) => handleAttributeChanging(cmd)
    case (ctx, Start(nBoidsText)) =>
      try
        validateNBoids(nBoidsText.toInt)
        start()
        ctx.self ! UpdateView
        running
      catch
        case _: NumberFormatException    => ctx.log info "Input format not allowed!";     Behaviors.same
        case _: IllegalArgumentException => ctx.log info "Only positive numbers allowed!";Behaviors.same

  private val running: Behavior[Loop] = Behaviors.receivePartial:
    case (ctx, UpdateView) => updateView
    case (ctx, cmd: ChangeAttribute) => handleAttributeChanging(cmd)
    case (ctx, UpdateBoids) =>
      (ctx spawnAnonymous updateBoids) ! Command.UpdateBoidsVel
      Behaviors.same
    case (ctx, Stop) =>
      stop()
      notRunning
    case (ctx, Suspend) =>
      suspend()
      suspended

  private val suspended: Behavior[Loop] = Behaviors.receivePartial:
    case (ctx, cmd: ChangeAttribute) => handleAttributeChanging(cmd)
    case (ctx, Stop)   =>
      view foreach (_.resumeAction())
      stop()
      notRunning
    case (ctx, Resume) =>
      resume()
      ctx.self ! UpdateView
      running

  private def stop(): Unit =
    view foreach {_.stopAction()}
    model.clearBoids()
    view foreach : (v: BoidsView) =>
      v.update()
      v.updateFrameRate(0)
      v.enableStartStopButton()

  def runSimulation(): Unit =
    mainLoop = Some(ActorSystem(notRunning, "MainLoop"))
    import SimulationParameter.*
    view = Some(BoidsView(model, SCREEN_WIDTH, SCREEN_HEIGHT, mainLoop.get))

  private val updateBoids: Behavior[Command] = Behaviors.receive: (ctx, cmd) =>
    cmd match
    case Command.UpdateBoidsVel =>
      model.boids foreach {_.updateVelocity(model)}
      ctx.self ! Command.UpdateBoidsPos
      Behaviors.same
    case Command.UpdateBoidsPos =>
      model.boids foreach {_.updatePos(model)}
      mainLoop foreach{_ ! UpdateView}
      Behaviors.stopped
}