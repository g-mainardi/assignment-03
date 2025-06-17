package pcd.ass01

class Boid(var pos: P2d, var vel: V2d) {
  import Attribute.*

  def updateVelocity(model: BoidsModel): Unit =
    val calc = calculate(getNearbyBoids(model), model)
    vel = vel sum Attribute.values.foldLeft(V2d(.0, .0)): (acc, attr) =>
      acc sum (calc(attr) mul (model getWeightOf attr))
    if vel.abs > model.maxSpeed then vel = vel.getNormalized mul model.maxSpeed

  def updatePos(model: BoidsModel): Unit =
    pos = pos sum vel
    if pos.x < model.getMinX  then pos = pos sum V2d(model.width, 0)
    if pos.x >= model.getMaxX then pos = pos sum V2d(-model.width, 0)
    if pos.y < model.getMinY  then pos = pos sum V2d(0, model.height)
    if pos.y >= model.getMaxY then pos = pos sum V2d(0, -model.height)

  private def getNearbyBoids(model: BoidsModel) = model.boids filter : other =>
    (other ne this) && posInRadius(other.pos, model.perceptionRadius)

  private def calculate(boids: List[Boid], model: BoidsModel)(a: Attribute): V2d = (a match
    case SEPARATION => calculateSeparation
    case ALIGNMENT  => calculateAlignment
    case COHESION   => calculateCohesion)(boids, model)

  private def calculateAll(getVector: Boid => Vector2d)(nearbyBoids: List[Boid], model: BoidsModel) =
    if nearbyBoids.nonEmpty
    then
      val vec = getVector(this)
      val (avgX, avgY) = nearbyBoids.map(getVector)
        .foldLeft((.0,.0)){(acc, other) => (acc._1 + other.x, acc._2 + other.y)}
      V2d(avgX / nearbyBoids.size - vec.x, avgY / nearbyBoids.size - vec.y).getNormalized
    else
      V2d(0, 0)

  private def calculateAlignment(nearbyBoids: List[Boid], model: BoidsModel) =
    calculateAll(_.vel)(nearbyBoids, model)

  private def calculateCohesion(nearbyBoids: List[Boid], model: BoidsModel) =
    calculateAll(_.pos)(nearbyBoids, model)

  private def calculateSeparation(nearbyBoids: List[Boid], model: BoidsModel) =
    nearbyBoids.map(_.pos).foldLeft((P2d(.0, .0), 0)) { (acc, other) =>
      if posInRadius(other, model.avoidRadius)
      then (acc._1 sum(pos sub other), acc._2 + 1)
      else acc
    } match
      case (_, 0)       => V2d(0,0)
      case (P2d(dx, dy), count) => V2d(dx / count, dy / count).getNormalized

  private def posInRadius(other: P2d, radius: Double): Boolean = radius > (pos distance other)
}