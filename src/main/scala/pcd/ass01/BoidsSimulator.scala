package pcd.ass01
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import pcd.ass01.BoidsModel.Attribute

object SimulationParameter:
  val N_BOIDS = 1000
  val SCREEN_WIDTH = 1280
  val SCREEN_HEIGHT = 480
  private[ass01] val SEPARATION_WEIGHT = 1.0
  private[ass01] val ALIGNMENT_WEIGHT = 1.0
  private[ass01] val COHESION_WEIGHT = 1.0
  private[ass01] val ENVIRONMENT_WIDTH = 1000
  private[ass01] val ENVIRONMENT_HEIGHT = 1000
  private[ass01] val MAX_SPEED = 4.0
  private[ass01] val PERCEPTION_RADIUS = 50.0
  private[ass01] val AVOID_RADIUS = 20.0

object BoidsSimulator :
  trait Loop
  enum UI extends Loop:
    case Start(nBoids: String)
    case Stop
    case Suspend
    case Resume
    case ChangeAttribute(a: Attribute, value: Double)
  enum Update extends Loop:
    case Boids
    case View

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
  import BoidsSimulator.*

  protected var mainLoop: Option[ActorSystem[Loop]] = None

  protected val start: Behavior[Loop] = Behaviors.setup: ctx =>
    view foreach {_.startAction()}
    model generateBoids ctx
    t0 = System.currentTimeMillis
    view foreach (_.enableStartStopButton())
    ctx.self ! Update.View
    running

  protected val notRunning: Behavior[Loop] = Behaviors.receivePartial:
    case (ctx, cmd: UI.ChangeAttribute) => handleAttributeChanging(cmd)
    case (ctx, UI.Start(nBoidsText)) =>
      try
        validateNBoids(nBoidsText.toInt)
        start
      catch
        case _: NumberFormatException    => ctx.log info "Input format not allowed!";     Behaviors.same
        case _: IllegalArgumentException => ctx.log info "Only positive numbers allowed!";Behaviors.same

  private val running: Behavior[Loop] = Behaviors.receivePartial:
    case (ctx, Update.View) =>
      updateView()
      ctx.self ! Update.Boids
      running
    case (ctx, cmd: UI.ChangeAttribute) => handleAttributeChanging(cmd)
    case (ctx, Update.Boids) => updateBoids
    case (ctx, UI.Stop) =>
      stop()
      notRunning
    case (ctx, UI.Suspend) =>
      suspend()
      suspended

  private val suspended: Behavior[Loop] = Behaviors.receivePartial:
    case (ctx, cmd: UI.ChangeAttribute) => handleAttributeChanging(cmd)
    case (ctx, UI.Stop)   =>
      view foreach (_.resumeAction())
      stop()
      notRunning
    case (ctx, UI.Resume) =>
      resume()
      ctx.self ! Update.View
      running

  private val updateBoids: Behavior[Loop] = Behaviors.setup: ctx =>
    ctx spawnAnonymous(model updateBoids(ctx, mainLoop.get))
    running

  private val validateNBoids: Int => Unit =
    case n if n > 0  => model setBoidsNumber n
    case _ => throw new IllegalArgumentException

  private val handleAttributeChanging: PartialFunction[Loop, Behavior[Loop]] =
    case UI.ChangeAttribute(attr, value) => model setWeight(attr, value); Behaviors.same

  private def stop(): Unit =
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