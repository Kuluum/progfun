val t = ((1000 to 10000) filter (_%3==0))(1) // bad performance

/**
  Avoid computing tail of a sequence until it is needed for the evaluation result
  (which might be never)

  Stream
  */

val xs = Stream.cons(1, Stream.cons(2, Stream.empty))

val xs2 = Stream(1, 2, 3)

(1 to 1000).toStream //res0: Stream[Int] = Stream(1, ?)

def streamRange(lo: Int, hi: Int): Stream[Int] =
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))

streamRange(5, 42) //res1: Stream[Int] = Stream(5, ?)

(1000 to 10000).toStream filter (_%3==0) //res2 ... = Stream(1002, ?)

/**
  x :: xs always produces a list, never a stream.
  */

2 :: List(3, 4) //res3: List[Int] = List(2, 3, 4)

/**
  #:: produces a stream.
  x #:: xs == Stream.cons(x, xs)
  */

2 #:: Stream(3, 8)//res4: Stream[Int] = Stream(2, ?)


trait CStream[+A] /*extends Seq[A]*/ {
  def isEmpty: Boolean
  def head: A
  def tail: CStream[A]
}

object CStream {
  def cons[T](hd: T, tl : => CStream[T]) = new CStream[T] {
    override def isEmpty = false
    override def head = hd
    override def tail = tl
  }

  val empty = new CStream[Nothing] {
    override def isEmpty = true
    override def head = throw new NoSuchElementException("empty.head")
    override def tail = throw new NoSuchElementException("empty.tail")
  }
}

def streamRange2(lo: Int, hi: Int): Stream[Int] = {
  print(lo + " ")
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange2(lo + 1, hi))
  }

streamRange2(1, 10).take(3).toList //1 2 3 res5: List[Int] = List(1, 2, 3)