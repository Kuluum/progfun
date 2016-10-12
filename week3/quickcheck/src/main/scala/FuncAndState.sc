def iterate(n: Int, f: Int => Int, x: Int): Int =
  if (n == 0) x else iterate(n-1, f, f(x))

def square(x: Int) = x * x


iterate(1, square, 3)

/*->*/if (1 == 0) 3 else iterate(1-1, square, square(3))
/*->*/iterate(0, square, square(3))
/*->*/iterate(0, square, 3 * 3)
/*->*/iterate(0, square, 9)
/*->*/if (0 == 0) 9 else iterate(0-1, square, square(9))
/*->*/9


/**
  State
  */

class BankAccount {
  private var balance = 0
  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
  }
  def widraw(amount: Int): Int = {
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      balance
    } else throw new Error("insufficient funds")
  }
}

val acc = new BankAccount

acc deposit 50
acc widraw 20 // 30
acc widraw 20 // 10
acc widraw 15 // Error

