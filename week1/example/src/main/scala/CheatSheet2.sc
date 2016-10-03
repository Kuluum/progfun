/**
Collections
  */

val fruitList = List("apples", "oranges", "pears")
// Alternative syntax for lists
val fruit = "apples" :: ("oranges" :: ("pears" :: Nil)) // parens optional, :: is right-associative
fruit.head   // "apples"
fruit.tail   // List("oranges", "pears")
val emptyL = List()
val empty = Nil

val nums = Vector("louis", "frank", "hiromi")
nums(1)                     // element at index 1, returns "frank", complexity O(log(n))
nums.updated(2, "helena")   // new vector with a different string at index 2, complexity O(log(n))

val fruitSet = Set("apple", "banana", "pear", "banana")
fruitSet.size    // returns 3: there are no duplicates, only one banana

val r: Range = 1 until 5 // 1, 2, 3, 4
val s: Range = 1 to 5    // 1, 2, 3, 4, 5
1 to 10 by 3  // 1, 4, 7, 10
6 to 1 by -2  // 6, 4, 2

val s2 = (1 to 6).toSet
s2 map(_ + 2) // adds 2 to each element of the set

val s3 = "Hello World"
s3 filter (c => c.isUpper) // returns "HW"; strings can be treated as Seq[Char]

// Operations on sequences
val xs = List(1,2,3,4,5,78)
xs.length   // number of elements, complexity O(n)
xs.last     // last element (exception if xs is empty), complexity O(n)
xs.init     // all elements of xs but the last (exception if xs is empty), complexity O(n)

val ys = List('a', 'b', 'c')
val n = 2
xs take n   // first n elements of xs
xs drop n   // the rest of the collection after taking n elements
xs(n)       // the nth element of xs, complexity O(n)
xs ++ ys    // concatenation, complexity O(n)
xs.reverse  // reverse the order, complexity O(n)
xs updated (2, 98)

xs indexOf 4      // the index of the first element equal to x (-1 otherwise)
xs contains 78     // same as xs indexOf x >= 0
xs filter (_>4)       // returns a list of the elements that satisfy the predicate p
xs filterNot (_>4)    // filter with negated p
xs partition (_>4)    // same as (xs filter p, xs filterNot p)
xs takeWhile (_<4)    // the longest prefix consisting of elements that satisfy p
xs dropWhile (_>4)    // the remainder of the list after any leading element satisfying p have been removed
xs span (_<4)         // same as (xs takeWhile p, xs dropWhile p)

List(1, 2, 3, 4) reduceLeft ((a, b) => a+b)  // (...(x1 op x2) op x3) op ...) op xn
List('a', 'b', 'c', 'd').foldLeft("")((a, b) => "("+a+b+")")  // (...( z op x1) op x2) op ...) op xn
List(1, 2, 3, 4) reduceRight (_-_)   // x1 op (... (x{n-1} op xn) ...)
List('a', 'b', 'c', 'd').foldRight("")("("+_+_+")") // x1 op (... (    xn op  z) ...)

//xs exists (_>21)    // true if there is at least one element for which predicate p is true
//xs forall (_>0)   // true if p(x) is true for all elements
val xys = xs zip ys      // returns a list of pairs which groups elements with same index together
val unzip = xys.unzip// opposite of zip: returns a pair of two lists
//xs.flatMap(p)   // applies the function to all elements and concatenates the result
xs.sum         // sum of elements of the numeric collection
xs.product     // product of elements of the numeric collection
xs.max         // maximum of collection
xs.min         // minimum of collection
List(List(1,2,3), List(4,5,6)).flatten     // flattens a collection of collection into a single-level collection
xs groupBy (_%2==0)   // returns a map which points to a list of elements
xs distinct    // sequence of distinct entries (removes duplicates)

val x = 92
x +: xs  // creates a new collection with leading element x
xs :+ x  // creates a new collection with trailing element x

// Operations on maps
val myMap = Map("I" -> 1, "V" -> 5, "X" -> 10)  // create a map
myMap("I")      // => 1
//myMap("A")      // => java.util.NoSuchElementException
myMap get "A"   // => None
myMap get "I"   // => Some(1)
myMap.updated("V", 15)  // returns a new map where "V" maps to 15 (entry is updated)
// if the key ("V" here) does not exist, a new entry is added

// Operations on Streams
val ss = Stream(1, 2, 3)
val ss2 = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty))) // same as above
(1 to 1000).toStream // => Stream(1, ?)
x #:: ss // Same as Stream.cons(x, xs)
// In the Stream's cons operator, the second parameter (the tail)
// is defined as a "call by name" parameter.
// Note that x::xs always produces a List


/**
  Pairs (similar for larger Tuples)
  */

val pair = ("answer", 42)   // type: (String, Int)
val (label, value) = pair   // label = "answer", value = 42
pair._1 // "answer"
pair._2 // 42

/**
  For-Comprehensions

  A for-comprehension is syntactic sugar for map, flatMap and filter operations on collections.

  The general form is for (s) yield e

  s is a sequence of generators and filters
  p <- e is a generator
  if f is a filter
  If there are several generators (equivalent of a nested loop), the last generator varies faster than the first
  You can use { s } instead of ( s ) if you want to use multiple lines without requiring semicolons
  e is an element of the resulting collection
  */

val M = 5
val N = 2
// list all combinations of numbers x and y where x is drawn from
// 1 to M and y is drawn from 1 to N
for (x <- 1 to M; y <- 1 to N)
  yield (x,y)
//is equivalent to
(1 to M) flatMap (x => (1 to N) map (y => (x, y)))


/**
  Translation Rules

  A for-expression looks like a traditional for loop but works differently internally

  for (x <- e1) yield e2 is translated to e1.map(x => e2)

  for (x <- e1 if f) yield e2 is translated to for (x <- e1.filter(x => f)) yield e2

  for (x <- e1; y <- e2) yield e3 is translated to e1.flatMap(x => for (y <- e2) yield e3)

  This means you can use a for-comprehension for your own type, as long as you define map, flatMap and filter.
  */

val k = 3

for {
  i <- 1 until k
  j <- 1 until i
  if (i + j) % 3 == 0
} yield (i, j)


for (i <- 1 until k; j <- 1 until i if (i + j)%3==0)
  yield (i, j)

(1 until k).flatMap(i => (1 until i).filter(j => (i + j)%3==0).map(j => (i, j)))         // evaluated when called