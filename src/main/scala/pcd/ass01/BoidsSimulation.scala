package pcd.ass01

object BoidsSimulation extends App:
  import SimulationParameter.*
  val model = BoidsModel(N_BOIDS, SEPARATION_WEIGHT, ALIGNMENT_WEIGHT,
                          COHESION_WEIGHT, ENVIRONMENT_WIDTH, ENVIRONMENT_HEIGHT,
                          MAX_SPEED, PERCEPTION_RADIUS, AVOID_RADIUS)

  BoidsSimulator(model).runSimulation()
