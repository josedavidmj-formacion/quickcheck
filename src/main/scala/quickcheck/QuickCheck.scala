package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def inOrder(k: H): Boolean = {
    if (isEmpty(k)) true
    else {
      val min = findMin(k)
      val tail = deleteMin(k)
      if (isEmpty(tail)) true
      else (min <= findMin(tail)) && inOrder(tail)
    }
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2ab") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val i = insert(b, h)
    (a < b) ==> (findMin(i) == a)
  }

  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("del2ba") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val i = insert(b, h)
    val j = deleteMin(i)
    (b < a) ==> (findMin(j) == a)
  }

  property("delgen") = forAll { (a: Int, h: H) =>
    val mini = findMin(h)
    val j = deleteMin(h)
    (isEmpty(j) || (mini <= findMin(j) && inOrder(j)))
  }

  property("del2symmetric") = forAll { (a: Int, b: Int) =>
    deleteMin(insert(b, insert(a, empty))) == deleteMin(insert(a, insert(b, empty)))
  }

  property("isEmpty0") = forAll { a: Int =>
    val h = insert(a, empty)
    val j = deleteMin(h)
    isEmpty(j)
  }

  property("isEmpty1") = forAll { a: Int =>
    val h = insert(a, empty)
    !isEmpty(h)
  }

  property("isEmpty1Gen") = forAll { (h: H) =>
    !isEmpty(h)
  }

  property("meld1") = forAll { a: Int =>
    val h = insert(a, empty)
    val i = meld(h, empty)
    findMin(i) == a
  }

  property("meldempty") = forAll { a: Int =>
    val i = meld(empty, empty)
    isEmpty(i)
  }

  property("meld1Gen") = forAll { (h: H) =>
    val i = meld(h, empty)
    findMin(i) == findMin(h)
  }

  property("meld2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val i = insert(b, empty)
    findMin(meld(h, i)) == findMin(meld(i, h))
  }

  property("meld2Gen") = forAll { (h: H, i: H) =>
    findMin(meld(h, i)) == findMin(meld(i, h))
  }

  property("meld2Gen2") = forAll { (h: H, i: H) =>
    val minh = findMin(h)
    val mini = findMin(i)
    val minmeld = findMin(meld(h, i))
    ((minh < mini) && (minmeld == minh)) || ((minh >= mini) && (minmeld == mini))
  }

  property("insertmingen") = forAll { (h: H) =>
    val minh = findMin(h)
    val j = insert(minh, h)
    (findMin(j) == minh) && inOrder(j)
  }

  property("insertminvalue") = forAll { (a: Int, h: H) =>
    val i = insert(a, h)
    (findMin(i) <= a) && inOrder(i)
  }

  property("meld2Either") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val i = insert(b, empty)
    val j = meld(h, i)
    val c = findMin(j)
    ((c == a) || (c == b)) && inOrder(j)
  }

  property("meld2EitherGen") = forAll { (h: H, i: H) =>
    val c = findMin(meld(h, i))
    ((c == findMin(h)) || (c == findMin(i)))
  }

  property("meld2Self") = forAll { (b: Int) =>
    val i = insert(b, empty)
    val j = meld(i, i)
    (findMin(j) == findMin(i)) && inOrder(j)
  }

  property("sorted") = forAll { (h: H) =>
    inOrder(h)
  }

  property("meld2SelfGen") = forAll { (h: H) =>
    val j = meld(h, h)
    (findMin(j) == findMin(h)) && inOrder(j)
  }

  property("deletemeldGen") = forAll { (h: H, i: H) =>
    val minh = findMin(h)
    val mini = findMin(i)
    val j = deleteMin(meld(h, i))
    val minmeld = findMin(j)
    ((minh <= minmeld) || (mini <= minmeld)) && inOrder(j)
  }
  
  property("deleteMinLeavesSortedHeap") = forAll { (a: Int, b: Int, c: Int, d: Int, e: Int) =>
    val h = insert(a, empty)
    val i = insert(b, h)
    val j = insert(c, i)
    val k0 = insert (d, j)
    val k = insert (e, k0)
    val n = findMin(k)
    val z = deleteMin(k)
    val y = insert(n, z)
    val w = meld(z, k)
    val v = meld(z, k0)
    val u = meld (z, z)
    val t = meld (z, i)
    val s = deleteMin(z)
    val r = insert(a, s)
    val q = insert (b, s)
    val p = insert (c, s)
    inOrder(z) && inOrder(y) && (findMin(y) == n) && inOrder(w) && inOrder(v) &&
    inOrder(u) && inOrder(t) && inOrder(s) && inOrder(r) && inOrder(q) && 
    inOrder(p)
    
  }
  
  property("deleteMinLeavesSortedHeap2") = forAll { (h: H) =>
    val a = findMin(h)
    val z = deleteMin(h)
    val y = if (isEmpty(z)) empty else deleteMin(z)
    val b = if (isEmpty(z)) a else findMin(z)
    val x = insert(a, z)
    inOrder(z) && inOrder(y) && (a <= b) && inOrder(x) && (findMin(x) == a)
  }

}
