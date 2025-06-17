package pcd.ass01

import pcd.ass01.Attribute.*

import javax.swing.*
import javax.swing.event.ChangeEvent
import javax.swing.event.ChangeListener
import java.awt.*
import java.util

object BoidsView {
  val START = "start"
  val STOP = "stop"
  val SUSPEND = "suspend"
  val RESUME = "resume"
}

private class SynchButton(text: String) extends JButton(text):
  override def setEnabled(b: Boolean): Unit = synchronized(super.setEnabled(b))
  override def isEnabled: Boolean = synchronized(super.isEnabled)

class MySlider(val attribute: Attribute) extends JSlider(SwingConstants.HORIZONTAL, 0, 20, 10):
  setMajorTickSpacing(10)
  setMinorTickSpacing(1)
  setPaintTicks(true)
  setPaintLabels(true)
  private val labelTable = new util.Hashtable[Integer, JLabel]
  labelTable put( 0, JLabel("0"))
  labelTable put(10, JLabel("1"))
  labelTable put(20, JLabel("2"))
  setLabelTable(labelTable)
  setPaintLabels(true)

object SeparationSlider extends MySlider(SEPARATION)
object CohesionSlider extends MySlider(COHESION)
object AlignmentSlider extends MySlider(ALIGNMENT)

class BoidsPanel(val panelWidth: Int, val panelHeight: Int, private val model: BoidsModel) extends JPanel:
  private var framerate = 0

  def setFrameRate(framerate: Int): Unit = this.framerate = framerate

  override protected def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    import scala.language.implicitConversions
    given Conversion[P2d, (Double, Double)] = pos => (pos.x, pos.y)

    setBackground(Color.WHITE)
    val xScale = panelWidth / model.width
    //    val yScale = height / model.height

    // Draw Boids
    g setColor Color.BLUE
    for
      boid <- model.boids
      (x, y) = boid.pos: (Double, Double)
      px = (panelWidth  / 2 + x * xScale).toInt
      py = (panelHeight / 2 - y * xScale).toInt
    do
      g fillOval(px, py, 5, 5)

    g setColor Color.BLACK
    g drawString(s"Num. Boids: ${model.nBoids}", 10, 25)
    g drawString(s"Framerate: $framerate", 10, 40)

class BoidsView(private val model: BoidsModel, val width: Int, val height: Int) extends ChangeListener {
  private val frame = new JFrame("Boids Simulation")
  frame.setSize(width, height)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  private val boidsPanel = BoidsPanel(width, height, model)

  private val cp = new JPanel
  cp setLayout new BorderLayout
  cp add(BorderLayout.CENTER, boidsPanel)
  cp add(BorderLayout.SOUTH, ControlPanel)

  SeparationSlider addChangeListener this
  CohesionSlider   addChangeListener this
  AlignmentSlider  addChangeListener this

  this.disableSuspendResumeButton()
  this.resetBoidsNumberField()

  frame setContentPane cp
  frame setVisible true

  def update(): Unit = boidsPanel.repaint()

  def updateFrameRate(frameRate: Int): Unit = boidsPanel setFrameRate frameRate

  private object ControlPanel extends JPanel:
    SuspendResumeButton addActionListener { _ => SuspendResumeButton.getText match
      case BoidsView.SUSPEND => suspendAction()
      case BoidsView.RESUME => resumeAction()
      case _ => ()
    }
    this add new JLabel("Separation")
    this add SeparationSlider
    this add new JLabel("Alignment")
    this add AlignmentSlider
    this add new JLabel("Cohesion")
    this add CohesionSlider
    this add new JLabel("Count")
    this add BoidsNumberField
    this add StartAndStopButton
    StartAndStopButton addActionListener { _ => StartAndStopButton.getText match
      case BoidsView.START =>
        try
          val newBoidsNumber = BoidsNumberField.getText.toInt
          if newBoidsNumber <= 0 then throw new IllegalArgumentException
          startAction(newBoidsNumber)
        catch
          case e: NumberFormatException => println("Input format not allowed!")
          case e: IllegalArgumentException => println("Only positive numbers allowed!")
      case BoidsView.STOP => stopAction()
      case _ => ()
    }
    this add SuspendResumeButton

  private object BoidsNumberField extends JTextField(5)

  private object StartAndStopButton extends SynchButton(BoidsView.START)
  private object SuspendResumeButton extends SynchButton(BoidsView.SUSPEND)

  private def stopAction(): Unit =
    this.disableStartAndStopButton()
    this.enableNumBoidsField()
    model.turnOff()
    this.resetBoidsNumberField()
    StartAndStopButton setText BoidsView.START
    this.disableSuspendResumeButton()

  private def startAction(newBoidsNumber: Int): Unit =
    this.disableStartAndStopButton()
    this.disableNumBoidsField()
    model setBoidsNumber newBoidsNumber
    model.turnOn()
    BoidsNumberField setText ""
    StartAndStopButton setText BoidsView.STOP
    this.enableSuspendResumeButton()

  def resumeAction(): Unit =
    this.disableSuspendResumeButton()
    SuspendResumeButton setText BoidsView.SUSPEND
    model.resume()

  private def suspendAction(): Unit =
    this.disableSuspendResumeButton()
    SuspendResumeButton setText BoidsView.RESUME
    model.suspend()

  def enableStartStopButton(): Unit = StartAndStopButton setEnabled true
  def enableSuspendResumeButton(): Unit = SuspendResumeButton setEnabled true
  private def enableNumBoidsField(): Unit = BoidsNumberField setEnabled true
  private def disableNumBoidsField(): Unit = BoidsNumberField setEnabled false
  private def disableStartAndStopButton(): Unit = StartAndStopButton setEnabled false
  private def disableSuspendResumeButton(): Unit = SuspendResumeButton setEnabled false
  private def resetBoidsNumberField(): Unit = BoidsNumberField setText SimulationParameter.N_BOIDS.toString

  override def stateChanged(e: ChangeEvent): Unit = e.getSource match
    case s:MySlider => model setWeight(s.attribute, s.getValue)
    case _ => ()
}