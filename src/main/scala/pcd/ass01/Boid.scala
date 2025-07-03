package pcd.ass01

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass01.BoidsModel.Command.{UpdatedBoidsPos, UpdatedBoidsVel}

object BoidActor:
  enum Command:
    case UpdateVel(model: BoidsModel, replyTo: ActorRef[BoidsModel.Command])
    case UpdatePos(model: BoidsModel, replyTo: ActorRef[BoidsModel.Command])
    case Kill

  def apply(boid: Boid): Behavior[Command] =
    Behaviors setup { ctx => new BoidActor(ctx, boid).waiting }

case class Boid(private var _pos: P2d, private var _vel: V2d):
  def pos: P2d = _pos
  def pos_=(p: P2d): Unit = _pos = p
  def vel: V2d = _vel
  def vel_=(v: V2d): Unit = _vel = v

private class BoidActor(ctx: ActorContext[BoidActor.Command], val boid: Boid) {
  import BoidsModel.Attribute
  import Attribute.*
  import BoidActor.Command.*

  export boid.{pos, vel}

  private val waiting: Behavior[BoidActor.Command] = Behaviors.receiveMessagePartial:
    case UpdateVel(model, replyTo) =>
      updateVelocity(model)
      replyTo ! UpdatedBoidsVel
      Behaviors.same
    case UpdatePos(model, replyTo) =>
      updatePos(model)
      replyTo ! UpdatedBoidsPos
      Behaviors.same
    case Kill => Behaviors.stopped

  private def updateVelocity(model: BoidsModel): Unit =
    val calc = calculate(getNearbyBoids(model), model)
    boid.vel = vel + Attribute.values.foldLeft(V2d(.0, .0)): (acc, attr) =>
      acc + (calc(attr) * (model getWeightOf attr))
    if vel.abs > model.maxSpeed then boid.vel = vel.norm * model.maxSpeed

  private def updatePos(model: BoidsModel): Unit =
    boid.pos = pos + vel
    if pos.x <  model.getMinX then boid.pos = pos + V2d(model.width, 0)
    if pos.x >= model.getMaxX then boid.pos = pos + V2d(-model.width, 0)
    if pos.y <  model.getMinY then boid.pos = pos + V2d(0, model.height)
    if pos.y >= model.getMaxY then boid.pos = pos + V2d(0, -model.height)

  private def getNearbyBoids(model: BoidsModel) = model.boids filter : other =>
    (other ne boid) && posInRadius(other.pos, model.perceptionRadius)

  private def calculate(boids: Seq[Boid], model: BoidsModel)(a: Attribute): V2d = (a match
    case SEPARATION => calculateSeparation
    case ALIGNMENT  => calculateAlignment
    case COHESION   => calculateCohesion)(boids, model)

  private def calculateAll(getVector: Boid => Vector2d)(nearbyBoids: Seq[Boid], model: BoidsModel) =
    if nearbyBoids.nonEmpty
    then
      val vec = getVector(boid)
      val (avgX, avgY) = nearbyBoids.map(getVector)
        .foldLeft((.0,.0)){(acc, other) => (acc._1 + other.x, acc._2 + other.y)}
      V2d(avgX / nearbyBoids.size - vec.x, avgY / nearbyBoids.size - vec.y).norm
    else
      V2d(0, 0)

  private def calculateAlignment(nearbyBoids: Seq[Boid], model: BoidsModel) =
    calculateAll(_.vel)(nearbyBoids, model)

  private def calculateCohesion(nearbyBoids: Seq[Boid], model: BoidsModel) =
    calculateAll(_.pos)(nearbyBoids, model)

  private def calculateSeparation(nearbyBoids: Seq[Boid], model: BoidsModel) =
    nearbyBoids.map(_.pos).foldLeft((P2d(.0, .0), 0)) { (acc, other) =>
      if posInRadius(other, model.avoidRadius)
      then (acc._1 +(pos - other), acc._2 + 1)
      else acc
    } match
      case (_, 0)       => V2d(0,0)
      case (P2d(dx, dy), count) => V2d(dx / count, dy / count).norm

  private def posInRadius(other: P2d, radius: Double): Boolean = radius > (pos distance other)
}