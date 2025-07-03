package pcd.ass01

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import pcd.ass01.BoidActor.Command.*
import pcd.ass01.BoidsSimulator.Loop

object BoidsModel:
  enum Command:
    case UpdatedBoidsVel
    case UpdatedBoidsPos

  enum Attribute:
    case SEPARATION
    case ALIGNMENT
    case COHESION

class BoidsModel(var nBoids: Int,
                 var separationWeight: Double,
                 var alignmentWeight: Double,
                 var cohesionWeight: Double,
                 val width: Double, val height: Double,
                 val maxSpeed: Double,
                 val perceptionRadius: Double,
                 val avoidRadius: Double) :
  import BoidsModel.*
  import Command.*

  var boids: Seq[Boid] = Seq()
  private var boidsRef: Seq[ActorRef[BoidActor.Command]] = Seq()
  private var simRef: Option[ActorRef[Loop]] = None

  def generateBoids(ctx: ActorContext[Loop]): Unit =
    boids =
      for
        i     <- 0 until nBoids
        origin = P2d(-width / 2, -height / 2)
        pos    = origin + V2d(Math.random * width, Math.random * height)
        rndVel = V2d(Math.random, Math.random) * (maxSpeed / 2)
        vel    = rndVel + v2d(- maxSpeed / 4)
      yield
        Boid(pos, vel)
    boidsRef =
      for
        (b, ind) <- boids.zipWithIndex
      yield
        ctx spawn (BoidActor(b), s"boid$ind")

  def clearBoids(): Unit =
    nBoids = 0
    boids = Seq()
    boidsRef foreach {_ ! Kill}
    boidsRef = Seq()

  def getMinX: Double = -width / 2
  def getMaxX: Double = width / 2
  def getMinY: Double = -height / 2
  def getMaxY: Double = height / 2

  def getWeightOf(a: Attribute): Double = a match
    case Attribute.SEPARATION => separationWeight
    case Attribute.ALIGNMENT  => alignmentWeight
    case Attribute.COHESION   => cohesionWeight

  def setWeight(a: Attribute, value: Double): Unit = a match
    case Attribute.SEPARATION => separationWeight = value
    case Attribute.ALIGNMENT  => alignmentWeight = value
    case Attribute.COHESION   => cohesionWeight = value

  def setBoidsNumber(n: Int): Unit = nBoids = n

  def updateBoids(ctx: ActorContext[Loop], replyTo: ActorRef[Loop]): Behavior[Command] = Behaviors.setup[Command]: ctx =>
    simRef = Some(replyTo)
    startUpdatingVel(ctx)

  private object NRemainingBoids:
    def unapply(n: Int): Option[Int] = if n > 0 then Some(n - 1) else None

  private def startUpdatingVel(ctx: ActorContext[Command]) =
    boidsRef foreach:
      _ ! UpdateVel(this, ctx.self)
    updatingVel(nBoids)

  private def updatingVel(remainingBoids: Int): Behavior[Command] = Behaviors.receivePartial:
    case (ctx, UpdatedBoidsVel) => remainingBoids match
      case NRemainingBoids(n) => n match
        case 0 => startUpdatingPos(ctx)
        case still => updatingVel(still)
      case _ => throw new IllegalStateException("Update Vel: boid number not valid!")

  private def startUpdatingPos(ctx: ActorContext[Command]) =
    boidsRef foreach:
      _ ! UpdatePos(this, ctx.self)
    updatingPos(nBoids)

  private def updatingPos(remainingBoids: Int): Behavior[Command] = Behaviors.receivePartial:
    case (ctx, UpdatedBoidsPos) => remainingBoids match
      case NRemainingBoids(n) => n match
        case 0 => endUpdatingPos
        case still => updatingPos(still)
      case _ => throw new IllegalStateException("Update Pos: boid number not valid!")

  private def endUpdatingPos: Behavior[Command] =
    simRef foreach:
      _ ! Loop.UpdateView
    Behaviors.stopped
