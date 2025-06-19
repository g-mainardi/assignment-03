package pcd.ass01

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
  var boids: List[Boid] = List()

  def generateBoids(): Unit =
    boids = (for
      _     <- 0 until nBoids
      origin = P2d(-width / 2, -height / 2)
      pos    = origin sum V2d(Math.random * width, Math.random * height)
      rndVel = V2d(Math.random, Math.random) mul (maxSpeed / 2)
      vel    = rndVel sum v2d(- maxSpeed / 4)
    yield
      Boid(pos, vel)).toList

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