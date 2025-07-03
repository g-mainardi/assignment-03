package pcd.ass01

import akka.actor.typed.{ActorRef, ActorSystem}
import pcd.ass01.BoidsModel.Attribute
import Attribute.*

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

private object MySlider:
  private val orientation = SwingConstants.HORIZONTAL
  private val minValue = 0
  private val maxValue = 20
  private val initValue = 10
  val factor = 0.1
  
class MySlider(val attribute: Attribute) 
  extends JSlider(MySlider.orientation, MySlider.minValue, MySlider.maxValue, MySlider.initValue):
  setMajorTickSpacing(10)
  setMinorTickSpacing(1)
  setPaintTicks(true)
  setPaintLabels(true)
  private val labelTable = new util.Hashtable[Integer, JLabel]
  scala.List(getMinimum, getValue, getMaximum) foreach{v => labelTable put(v, JLabel(v.toString))}
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

import BoidsSimulator.Loop
class BoidsView(private val model: BoidsModel, val width: Int, val height: Int, private val mainLoop: ActorSystem[Loop]) extends ChangeListener {
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
      case BoidsView.SUSPEND => mainLoop ! Loop.Suspend
      case BoidsView.RESUME  => mainLoop ! Loop.Resume
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
      case BoidsView.START => mainLoop ! Loop.Start(BoidsNumberField.getText)
      case BoidsView.STOP  => mainLoop ! Loop.Stop
      case _ => ()
    }
    this add SuspendResumeButton

  private object BoidsNumberField extends JTextField(5)

  private object StartAndStopButton extends SynchButton(BoidsView.START)
  private object SuspendResumeButton extends SynchButton(BoidsView.SUSPEND)

  def stopAction(): Unit =
    this.disableStartAndStopButton()
    this.resetBoidsNumberField()
    this.enableNumBoidsField()
    StartAndStopButton setText BoidsView.START
    this.disableSuspendResumeButton()

  def startAction(): Unit =
    this.disableStartAndStopButton()
    this.disableNumBoidsField()
    BoidsNumberField setText ""
    StartAndStopButton setText BoidsView.STOP
    this.enableSuspendResumeButton()

  def resumeAction(): Unit =
    this.disableSuspendResumeButton()
    SuspendResumeButton setText BoidsView.SUSPEND

  def suspendAction(): Unit =
    this.disableSuspendResumeButton()
    SuspendResumeButton setText BoidsView.RESUME

  def enableStartStopButton(): Unit = StartAndStopButton setEnabled true
  def enableSuspendResumeButton(): Unit = SuspendResumeButton setEnabled true
  private def enableNumBoidsField(): Unit = BoidsNumberField setEnabled true
  private def disableNumBoidsField(): Unit = BoidsNumberField setEnabled false
  private def disableStartAndStopButton(): Unit = StartAndStopButton setEnabled false
  private def disableSuspendResumeButton(): Unit = SuspendResumeButton setEnabled false
  private def resetBoidsNumberField(): Unit = BoidsNumberField setText SimulationParameter.N_BOIDS.toString

  override def stateChanged(e: ChangeEvent): Unit = e.getSource match
    case s:MySlider if !s.getValueIsAdjusting => mainLoop ! Loop.ChangeAttribute(s.attribute, MySlider.factor * s.getValue)
    case _ => ()
}