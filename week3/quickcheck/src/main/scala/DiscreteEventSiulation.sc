/**
  Digital Circuits
  */



/**
  Discrete Event Simulation
  perforns actions, specefied by the user at a given moment.
  An action is a function that dosen't take any parameters and whick
  returns Unit:

  The time is simulated; it has nothing to with the actual time.
  */

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

class Circut extends Simulation {

  val InvertDelay = 1
  val OrDelay = 3
  val AndDelay = 4

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
      afterDelay(AndDelay) { output setSignal  (in1Sig & in2Sig}
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
}
