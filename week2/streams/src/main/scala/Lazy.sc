def expr = {
  val x = { print("x"); 1}
  lazy val y = {print("y"); 2}
  def z = {print("z"); 3}

  z + y + x + z + y + x

}

expr //xzyz

((1000 to 10000).toStream filter (_%3==0)) apply 1


/**
  Call-by-name
  Call-by-value

  Call-by-value functions compute the passed-in expression's value
  before calling the function, thus the same value is accessed every time.
  However, call-by-name functions recompute the passed-in expression's value
  every time it is accessed.
  */

def something() = {
  println("calling something")
  1 // return value
}

def callByValue(x: Int) = {
  println("x1=" + x)
  println("x2=" + x)
}

def callByName(x: => Int) = {
  println("x1=" + x)
  println("x2=" + x)
}

callByName(something())
callByValue(something())

/**
  scala> callByValue(something())
  calling something
  x1=1
  x2=1

  scala> callByName(something())
  calling something
  x1=1
  calling something
  x2=1
  */