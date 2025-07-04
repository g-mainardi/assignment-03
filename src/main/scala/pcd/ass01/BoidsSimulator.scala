package pcd.ass01
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import pcd.ass01.BoidsModel.Attribute

object BoidsSimulator :
  enum Loop:
    case Start(nBoids: String)
    case Stop
    case Suspend
    case Resume
    case UpdateBoids
    case UpdateView
    case ChangeAttribute(a: Attribute, value: Double)

trait SuspendableViewController:

  protected var view: Option[BoidsView] = None
  protected var framerate = 0
  protected var t0 = 0L

  private val maxFramerate = 50
  private val frameratePeriod = maxFramerate.getPeriod

  protected def suspend(): Unit =
    view foreach(_.suspendAction())
    view foreach(_.enableSuspendResumeButton())

  protected def resume(): Unit =
    view foreach(_.resumeAction())
    view foreach(_.enableSuspendResumeButton())

  protected def updateView(): Unit =
    view foreach : v =>
      v.update()
      v updateFrameRate framerate
      System.currentTimeMillis - t0 match
        case dt if dt < frameratePeriod =>
          try Thread sleep (frameratePeriod - dt)
          catch
            case ignore: Exception => ()
          framerate = maxFramerate
        case dt =>
          framerate = dt.getPeriod
      t0 = System.currentTimeMillis

  extension (dt: Long)
    private def getPeriod: Int = (1000 / dt).toInt

abstract class AbsBoidsSimulator(private val model: BoidsModel) extends SuspendableViewController:
  import BoidsSimulator.Loop
  import Loop.*

  protected var mainLoop: Option[ActorSystem[Loop]] = None

  protected val start: Behavior[Loop] = Behaviors.setup: ctx =>
    view foreach {_.startAction()}
    model generateBoids ctx
    t0 = System.currentTimeMillis
    view foreach (_.enableStartStopButton())
    ctx.self ! UpdateView
    running

  protected val notRunning: Behavior[Loop] = Behaviors.receivePartial:
    case (ctx, cmd: ChangeAttribute) => handleAttributeChanging(cmd)
    case (ctx, Start(nBoidsText)) =>
      try
        validateNBoids(nBoidsText.toInt)
        start
      catch
        case _: NumberFormatException    => ctx.log info "Input format not allowed!";     Behaviors.same
        case _: IllegalArgumentException => ctx.log info "Only positive numbers allowed!";Behaviors.same

  protected val running: Behavior[Loop] = Behaviors.receivePartial:
    case (ctx, UpdateView) =>
      updateView()
      ctx.self ! Loop.UpdateBoids
      running
    case (ctx, cmd: ChangeAttribute) => handleAttributeChanging(cmd)
    case (ctx, UpdateBoids) => updateBoids
    case (ctx, Stop) =>
      stop()
      notRunning
    case (ctx, Suspend) =>
      suspend()
      suspended

  protected val suspended: Behavior[Loop] = Behaviors.receivePartial:
    case (ctx, cmd: ChangeAttribute) => handleAttributeChanging(cmd)
    case (ctx, Stop)   =>
      view foreach (_.resumeAction())
      stop()
      notRunning
    case (ctx, Resume) =>
      resume()
      ctx.self ! UpdateView
      running

  protected val updateBoids: Behavior[Loop] = Behaviors.setup: ctx =>
    ctx spawnAnonymous(model updateBoids(ctx, mainLoop.get))
    running

  private val validateNBoids: Int => Unit =
    case n if n > 0  => model setBoidsNumber n
    case _ => throw new IllegalArgumentException

  private val handleAttributeChanging: PartialFunction[Loop, Behavior[Loop]] =
    case ChangeAttribute(attr, value) => model setWeight(attr, value); Behaviors.same

  protected def stop(): Unit =
    view foreach {_.stopAction()}
    model.clearBoids()
    view foreach : (v: BoidsView) =>
      v.update()
      v.updateFrameRate(0)
      v.enableStartStopButton()

class BoidsSimulator(private val model: BoidsModel) extends AbsBoidsSimulator(model):
  def runSimulation(): Unit =
    mainLoop = Some(ActorSystem(notRunning, "MainLoop"))
    import SimulationParameter.*
    view = Some(BoidsView(model, SCREEN_WIDTH, SCREEN_HEIGHT, mainLoop.get))