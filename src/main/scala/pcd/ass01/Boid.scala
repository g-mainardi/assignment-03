package pcd.ass01

class Boid(private var _pos: P2d, private var _vel: V2d) {
  import Attribute.*
  def pos: P2d = _pos
  def pos_=(p: P2d): Unit = _pos = p
  def vel: V2d = _vel
  def vel_=(v: V2d): Unit = _vel = v

  def updateVelocity(model: BoidsModel): Unit =
    val calc = calculate(getNearbyBoids(model), model)
    vel = vel + Attribute.values.foldLeft(V2d(.0, .0)): (acc, attr) =>
      acc + (calc(attr) * (model getWeightOf attr))
    if vel.abs > model.maxSpeed then vel = vel.norm * model.maxSpeed

  def updatePos(model: BoidsModel): Unit =
    pos = pos + vel
    if pos.x < model.getMinX  then pos = pos + V2d(model.width, 0)
    if pos.x >= model.getMaxX then pos = pos + V2d(-model.width, 0)
    if pos.y < model.getMinY  then pos = pos + V2d(0, model.height)
    if pos.y >= model.getMaxY then pos = pos + V2d(0, -model.height)

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
      then (acc._1 +(pos - other), acc._2 + 1)
      else acc
    } match
      case (_, 0)       => V2d(0,0)
      case (P2d(dx, dy), count) => V2d(dx / count, dy / count).norm

  private def posInRadius(other: P2d, radius: Double): Boolean = radius > (pos distance other)
}