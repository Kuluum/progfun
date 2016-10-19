import org.scalacheck._
import org.scalacheck.Prop.forAll

/**
  forAll introducion
  */

val propConcatLists = forAll { (l1: List[Int], l2: List[Int]) =>
  l1.size + l2.size == (l1 ::: l2).size }
propConcatLists.check


val propSqrt = forAll { (n: Int) => scala.math.sqrt(n*n) == n }

propSqrt.check


val propReverseList = forAll { l: List[String] => l.reverse.reverse == l }

propReverseList.check

val propConcatString = forAll { (s1: String, s2: String) =>
  (s1 + s2).endsWith(s2)
}

propConcatString.check

/**
  Data generator
  */

val smallInteger = Gen.choose(0, 100)

val propSmallInteger = forAll(smallInteger) {n =>
  n >=0 && n <= 100
}

propSmallInteger.check

val myGen = for {
  n <- Gen.choose(10, 20)
  m <- Gen.choose(2*n, 500)
} yield (n,m)



val vowel = Gen.oneOf('A', 'E', 'I', 'O', 'S')

val vowel2 = Gen.frequency(
  (3, 'A'),
  (4, 'E'),
  (2, 'I'),
  (3, 'O'),
  (1, 'U'),
  (1, 'Y')
)

vowel2.sample
// Now, the vowel2 generator will generate E:s more often than Y:s.
// Roughly, 4/14 of the values generated will be E:s, and 1/14 of them will be Y:s.

// Generating Case Classes

sealed abstract class Tree
case class Node(left: Tree, right: Tree, v: Int) extends Tree
case object Leaf extends Tree

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

val genLeaf = const(Leaf)

val genNode = for {
  v <- arbitrary[Int]
  left <- genTree
  right <- genTree
} yield Node(left, right, v)

def genTree: Gen[Tree] = oneOf(genLeaf, genNode)

genTree.sample

/**
  Conditional Properties
  */

import org.scalacheck.Prop.BooleanOperators
val propMakeList = forAll { n: Int =>
  (n>= 0 && n < 1000) ==> (List.fill(n)("").length == n)
}



