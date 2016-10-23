import calculator.Signal

/*
class BankAccount {
  var balance = Var(0)

  def currentBalance = balance

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      balance() += amount
    }

  def widraw(amount: Int): Unit =
    if (0<amount && amount<=balance()) {
      balance() -= amount
    }
    else
      throw new Error("insuffucuent founds")
}

def consolidated(accts: List[BankAccount]): Signal[Int] =
  Signal(accts.map(_.balance()).sum)


val a = new BankAccount()
val b = new BankAccount()
val c = consolidated(List(a, b))

c()
a deposit 20
c()
b deposit 30
c()
*/

val s = new Signal(0)

s() = + 1
