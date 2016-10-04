abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

case class NonEmpty(elem: Int, left: IntSet, right:IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) NonEmpty(elem, left incl x, right)
    else if (x > elem) NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    (left union (right union (other))) incl elem
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other
}

/**
  To prove the correctness of this implementation we should define laws that it respects.

  In the case of IntSet (incl, contains), we have the following three laws:

  For any set s, and elements x and y:

  Empty contains x = false
  (s incl x) contains x = true
  (s incl x) contains y = s contains y    if x != y

  (In fact, we can show that these laws completely characterize the desired data type.)
  */

/**
  Proposition 1: Empty contains x = false
  Proof: According to the definition of contains in Empty.
  */

val x = 42
val y = 32
val z = 52
Empty contains x // res0: Boolean = true

/**
  Proposition 2: (s incl x) contains x = true
  Proof: By structural induction on s.
  Base case: Empty
    (Empty incl x) contains x = NonEmpty(xm Empty, Empty) contains x
  = true
  */

(Empty incl x) contains x //res1: Boolean = true

/**
  Induction step: NonEmpty(x, l, r)
    (NonEmpty(x, l, r) incl x) contains x)
  = NonEmpty(x, l, r) contains x
  = true
 */

val l = NonEmpty(y, Empty, Empty)
val r = NonEmpty(z, Empty, Empty)

(NonEmpty(x, l, r) incl x) contains x //res2: Boolean = true

/**
  Induction step: NonEmpty(y, l, r) where y < x
    (NonEmpty(y, l, r) incl x) contains x
  = NonEmpty(y,l,r incl x) contains x
  = (r incl x) contains x
  = true
  */

(NonEmpty(y, l, r) incl x) contains x //res3: Boolean = true

/**
  Induction step: NonEmpty(z, l, r) where z > x is analogous
  */

(NonEmpty (z, l, r) incl x) contains x //res4: Boolean = true

/**
  Position 3: If x != y then
  (xs incl y) contains x = xs contains x

  Proof: By structural induction on s. Assume that y < x (the dual case x < y is analogous).
  */

/**
  Base case: Empty

    (Empty incl y) contains x
  = NonEmpty(y, Empty, Empty) contains x
  = Empty contains x
  */

(Empty incl y) contains x // res5: Boolean = false

/**
  For inductive step, we need to consider a tree NonEmpty(z, l, r).
  We distinguish five cases:

  1. z = x
  2. z = y
  3. z < y < x
  4. y < z < x
  5. y < x < z
  */

/**
  1. NonEmpty (x, l, r)
    (NonEmpty(x, l, r) incl y) contains x
  = NonEmpty(x, l, incl y, r) contains x
  = true
  */

(NonEmpty(x, l, r) incl y) contains x //res6: Boolean = true


/**
  2. NonEmpty (y, l, r)
    (NonEmpty(y, l, r) incl y) contains x
  = NonEmpty(y, l, r) contains x
  =
  */

((NonEmpty(y, l, r) incl y) contains x) == (NonEmpty(y, l, r) contains x) //res7: Boolean = true


/**
  3. NonEmpty(z, l, r) where z < y < x
    (NonEmpty(z, l, r) incl y) contains x
  = NonEmpty(z, l, r incl y) contains x
  = (r incl y) contains x
  = r contains x
  = NonEmpty(z, l, r) contains x

  4. NonEmpty(z, l, r) where y< z < x
    (NonEmpty(z, l, r) incl y) contains x
  = NonEmpty(z, l incl y, r) contains x
  = r contains x
  = NonEmpty(z, l, r) contains x

  5.  NonEmpty(z, l, r) where y < x < z
    (NonEmpty(z, l, r) incl y) contains x
  = NonEmpty(z, l incl y, r) contains x
  = (l incl y) contains x
  = l contains x
  = NonEmpty(z, l, r) contains x
  */

/**
  These are all the cases, so the proposition is established.
  */