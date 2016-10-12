def power (x:Double, exp: Int): Double = {
  var r = 1.0
  var i = exp
  while (i>0) {r = r * x; i = i -1}
  r
}

def WHILE(condition: => Boolean)(command: => Unit): Unit = {
  if (condition) {
    command
    WHILE(condition)(command)
  }
  else ()
}


def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
  command
  if(condition) ()
  else REPEAT(command)(condition)
}

/**
  For-loops translate similary to for-expressions,
  but using the foreach combinator instead of map and flatMap
  */

for (i <- 1 until 3; j <- "abc") println(i + " " + j)

// translates to:

(1 until 3) foreach (i => "abc" foreach (j => println(i + " " + j)))