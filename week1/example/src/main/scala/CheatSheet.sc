import scala.collection.GenTraversable

/*
 Evaluation Rules
  */

def example1 = 2      // evaluated when called
val example2 = 2      // evaluated immediately
lazy val example3 = 2 // evaluated once when needed

def square(x: Double) = x*x    // call by value
def square2(x: => Double) = x*x // call by name
def myFct(bindings: Int*) = bindings // bindings is a sequence of int, containing a varying # of arguments

/**
 Higher order functions
  */

// sum() returns a function that takes two integers and returns an integer
def sum(f: Int => Int): (Int, Int) => Int = {
  def sumf(a: Int, b: Int): Int = f(a) + f(b)
  sumf
}

// same as above. Its type is (Int => Int) => (Int, Int) => Int
def sum2(f: Int => Int)(a: Int, b: Int): Int = f(a) + f(b)

// Called like this
sum((x: Int) => x * x * x)          // Anonymous function, i.e. does not have a name
sum(x => x * x * x)                 // Same anonymous function with type inferred


def cube(x: Int) = x * x * x
sum(x => x * x * x)(1, 10) // sum of  1 and 10
sum(cube)(1, 10)           // same as above

/**
  Currying
  Converting a function with multiple arguments into a function with a single argument that returns another function.
 */

def f(a: Int, b: Int): Int = a * b// uncurried version (type is (Int, Int) => Int)
def fCurry(a: Int)(b: Int): Int = a * b// curried version (type is Int => Int => Int)
val fCurry2: (Int => Int) = fCurry(_)(2)

fCurry2(3)

/**
  Classes
 */

class MyClass(x: Int, y: Int) {           // Defines a new type MyClass with a constructor
  require(y > 0, "y must be positive")    // precondition, triggering an IllegalArgumentException if not met

  def nb1 = x                             // public method computed every time it is called
  def nb2 = y
  val nb3 = x + y                         // computed only once

  private def test(a: Int): Int = 0 // private method

  override def toString = {
    // overridden method
    nb1 + ", " + nb2
  }
}

new MyClass(1, 2) // creates a new object of type

/**
  Class hierarchies
  */

abstract class TopLevel {
  // abstract class
  def method1(x: Int): Int

  // abstract method
  def method2(x: Int): Int = x * x
}

class Level1 extends TopLevel {
  def method1(x: Int): Int = 2 * x

  override def method2(x: Int): Int = x * x * x // TopLevel's method2 needs to be explicitly overridden
}

new Level1().method1(2)
new Level1().method2(2)

/**
  Type parameters
  Similar to C++ templates or Java generics. These can apply to classes, traits or functions.
   */

class MyClass2[T](arg1: T) {
  override def toString = arg1.toString()
}
new MyClass2[Int](1)
new MyClass2(1.6)   // the type is being inferred, i.e. determined based on the value arguments

/**
  Variance

  Given A <: B

  If C[A] <: C[B], C is covariant

  If C[A] >: C[B], C is contravariant

  Otherwise C is nonvariant

  */

/**
  For a function, if A2 <: A1 and B1 <: B2, then A1 => B1 <: A2 => B2.

  Functions must be contravariant in their argument types and covariant in their result types, e.g.
  */
trait Function1[-T, +U] {
  def apply(x: T): U
} // Variance check is OK because T is contravariant and U is covariant

//class Array[+T] {
//  def update(x: T)
//} // variance checks fails

/**
  Pattern Matching
  Pattern matching is used for decomposing data structures:
  */

List(1, 2, 3) match {
  case Nil => Nil          // empty list
  case x :: Nil => x       // list with only one element
//  case List(x) => x      // same as above
  //case x :: xs => xs      // a list with at least one element. x is bound to the head,
  // xs to the tail. xs could be Nil or some other list.
  case 1 :: 2 :: cs => cs // lists that starts with 1 and then 2
  //case (x, y) :: ps => (x, y) // a list where the head element is a pair
  case _ => Nil            // default case if none of the above matches
}

/**
  Options
  Pattern matching can also be used for Option values. Some functions (like Map.get) return a value of type
  Option[T] which is either a value of type Some[T] or the value None:
  */

val myMap = Map("a" -> 42, "b" -> 43)
def getMapValue(s: String): String = {
  myMap get s match {
    case Some(nb) => "Value found: " + nb
    case None => "No value found"
  }
}
getMapValue("a")  // "Value found: 42"
getMapValue("c")  // "No value found"

def getMapValue2(s: String): String =
  myMap.get(s).map("Value found: " + _).getOrElse("No value found")

getMapValue2("a")  // "Value found: 42"
getMapValue2("c")  // "No value found"


