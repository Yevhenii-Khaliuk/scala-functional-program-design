package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:

  private def toSequence(heap: H): List[A] =
    @tailrec
    def toSeq(h: H, acc: List[A]): List[A] =
      if isEmpty(h) then
        acc
      else
        val a = findMin(h)
        toSeq(deleteMin(h), acc :+ a)

    toSeq(heap, List.empty)

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a, h)
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap " +
    "should get the smallest of the two elements back") =
    forAll(const(empty), arbitrary[A], arbitrary[A]) { (h: H, a: A, b: A) =>
      val min = if a < b then a else b
      findMin(insert(b, insert(a, h))) == min
    }

  property("If you insert an element into an empty heap, then delete the minimum, " +
    "the resulting heap should be empty") =
    forAll(const(empty), arbitrary[A]) { (h: H, a: A) =>
      deleteMin(insert(a, h)) == empty
    }

  property("given any heap, you should get a sorted sequence of elements " +
    "when continually finding and deleting minima") =
    def ordered(l: List[A]): Boolean = l == l.sorted

    forAll { (h: H) =>
      ordered(toSequence(h))
    }

  property("finding a minimum of the melding of any two heaps should return a minimum of one or the other") =
    forAll(genHeap, genHeap) { (h1: H, h2: H) =>
      val h = meld(meld(h1, h2), meld(h1, h2))
      if isEmpty(h1) && isEmpty(h2) then
        true
      else if isEmpty(h1) then
        findMin(h) == findMin(h2)
      else if isEmpty(h2) then
        findMin(h) == findMin(h1)
      else
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        val min = if min1 <= min2 then min1 else min2
        findMin(h) == min
    }

  property("melding two heaps should result in heap containing elements of both heaps") =
    forAll(genHeap, genHeap) { (h1: H, h2: H) =>
      val expected = toSequence(h1) ++ toSequence(h2)
      val actual = toSequence(meld(h1, h2))
      actual.sorted == expected.sorted
    }
