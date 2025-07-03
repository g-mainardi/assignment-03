package pcd.ass01

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.ActorContext
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

  def generateBoids(ctx: ActorContext[Loop]): Unit =
    boids =
      for
        i <- 0 until nBoids
        origin = P2d(-width / 2, -height / 2)
        pos = origin + V2d(Math.random * width, Math.random * height)
        rndVel = V2d(Math.random, Math.random) * (maxSpeed / 2)
        vel = rndVel + v2d(-maxSpeed / 4)
      yield
        Boid(pos, vel)

  def clearBoids(): Unit =
    nBoids = 0
    boids = List()

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

  def updateBoids(ctx: ActorContext[Loop], replyTo: ActorRef[Loop]): Behavior[Command] = ???
