import org.scalacheck.Gen.Parameters

/**
  * Digital Circuits
  */



/**
  Discrete Event Simulation
  perforns actions, specefied by the user at a given moment.
  An action is a function that dosen't take any parameters and whick
  returns Unit:

  The time is simulated; it has nothing to with the actual time.
  */
trait Parameters {
  def InvertDelay: Int  = 1
  def OrDelay: Int  = 3
  def AndDelay: Int = 4
}

trait Simulation {
  type Action = () => Unit
  case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List()
  private var curtime = 0

  def currentTime: Int = curtime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if first.time <= item.time =>
      first :: insert(rest, item)
    case _ =>
      item :: ag
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }

  def run(): Unit = {
    afterDelay(0) {
      println("** simulation started, time = "+currentTime+" ***")
    }
    loop()
  }
}

trait Gates extends  Simulation {

  def InvertDelay: Int
  def AndDelay: Int
  def OrDelay: Int

  class Wire() {
    private var sigVal = false;
    private var actions: List[Action] = List()

    def getSignal: Boolean = sigVal
    def setSignal(sig: Boolean): Unit =
      if (sig != sigVal) {
        sigVal = sig
        actions foreach (_())
      }
    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InvertDelay) { output setSignal !inputSig}
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val in1Sig = a1.getSignal
      val in2Sig = a2.getSignal
      afterDelay(AndDelay) { output setSignal  (in1Sig & in2Sig)}
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(o1: Wire, o2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = o1.getSignal
      val in2Sig = o2.getSignal
      afterDelay(OrDelay) { output setSignal (in1Sig | in2Sig) }
    }
    o1 addAction orAction
    o2 addAction orAction
  }

  def orGateAlt(in1: Wire, in2: Wire, output: Wire): Unit = {
    val notIn1, notIn2, notOut = new Wire
    inverter(in1, notIn1); inverter(in2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }
}

trait Circut extends Gates {

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d = new Wire
    val e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fulAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s = new Wire
    val c1 = new Wire
    val c2 = new Wire
    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }
    wire addAction probeAction
  }

}


println("Welcome to the Simulation")
object sim extends Circut with Parameters
import sim._

val in1, in2, sum, carry = new Wire

halfAdder(in1, in2, sum, carry)
probe("sum", sum)
probe("carry", carry)

in1 setSignal true
run()
in2 setSignal true
run()

in1 setSignal false
run()