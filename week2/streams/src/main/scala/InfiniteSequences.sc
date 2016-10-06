def from(n: Int): Stream[Int] = n #:: from(n+1)

val nats = from(0)

val m4s = nats map (_ * 4)

(m4s take 10).toList //res0: List[Int] = List(0, 4, 8, ...)


/**
  The Sieve of Eratosthenes is an ancient technique to calculate prime numbers.

  The idea follows:
    Start with all integers from 2, the first prime number.
    Eliminate all multiples of 2.
    The first element of the resulting list is 3, a prime number.
    Eliminate all multiples of 3.
    Iterate forever. At each step, the first number in the list is a prime number and we eliminate all its multiples.
  */

def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_% s.head != 0))

val primes = sieve(from(2))

primes.take(100).toList


/**
  Square root algorithm on streams
  */

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(2).take(5).toList

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess * guess - x) / x) < 0.001


sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList
