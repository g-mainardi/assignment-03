package pcd.ass01

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

object BoidsSimulation extends App:
  import SimulationParameter.*
  val model = BoidsModel(N_BOIDS, SEPARATION_WEIGHT, ALIGNMENT_WEIGHT,
                          COHESION_WEIGHT, ENVIRONMENT_WIDTH, ENVIRONMENT_HEIGHT,
                          MAX_SPEED, PERCEPTION_RADIUS, AVOID_RADIUS)

  BoidsSimulator(model).runSimulation()
