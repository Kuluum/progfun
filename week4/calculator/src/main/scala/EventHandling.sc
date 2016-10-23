trait Subscriber {
  def handler(pub: Publisher)
}

trait Publisher {

  private var subscribers: Set[Subscriber] = Set()

  def subscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber

  def unsubscribe(subscriber: Subscriber): Unit =
    subscribers -= subscriber

  def publish(): Unit =
    subscribers.foreach(_.handler(this))
}


class BankAccount extends Publisher {
  private var balance = 0

  def currentBalance = balance

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      balance += amount
      publish()
    }

  def widraw(amount: Int): Unit =
    if (0<amount && amount<=balance) {
      balance -= amount
      publish()
    }
    else
      throw new Error("insuffucuent founds")
}

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute()

  private def compute() =
    total = observed.map(_.currentBalance).sum

  def handler(pub: Publisher) = compute()

  def totalBalance = total
}

val a = new BankAccount
val b = new BankAccount
val c = new Consolidator(List(a, b))

c.totalBalance
a.deposit(20)
c.totalBalance
b deposit 40
c.totalBalance

