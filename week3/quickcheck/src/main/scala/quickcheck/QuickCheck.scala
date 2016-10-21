package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- frequency((1, empty), (9, genHeap))
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("findMin after insert min") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin after insert arbitrary") = forAll { (h: H) =>
    forAll{ (i: Int) =>
      val heapMin = findMin(h)
      findMin(insert(i, h)) == (if(ord.lteq(heapMin, i)) heapMin else i)
    }
  }

  property("findMin after insert 2 ints") = forAll { (i: Int, j: Int) =>
    val heap = insert(j, insert(i, empty))
    findMin(heap) == math.min(i, j)
  }

  property("deleteMin of 1 elem heap") = forAll {(i: Int) =>
    val oneHeap = insert(i, empty)
    deleteMin(oneHeap) == empty
  }

  property("deleteMin of 2 elem heap") = forAll { (i: Int, j: Int) =>
    val heap = deleteMin(insert(j, insert(i, empty)))
    findMin(heap) == math.max(i, j)
  }

  property("findMin greater after deleteMin") = forAll { (h: H) =>
    val min = findMin(h)
    val newHeap = deleteMin(h)
    if (!isEmpty(newHeap))
      min <= findMin(deleteMin(h))
    else
      true
  }

  property("check heap findMin-deleteMin sorted sequence") = forAll{ (h: H) =>
   def checker(h: H, lastMin: Int): Boolean = {
     if(isEmpty(h)) true
     else {
       val newMin = findMin(h)
       if (newMin >= lastMin) checker(deleteMin(h), newMin) else false
     }
   }
    checker(h, findMin(h))
  }

  property("findMin of melded heaps") = forAll { (h1:H, h2:H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    findMin(meld(h1, h2)) == math.min(min1, min2)
  }

  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = remMin(meld1, Nil)
    val xs2 = remMin(meld2, Nil)
    xs1 == xs2
  }
}
