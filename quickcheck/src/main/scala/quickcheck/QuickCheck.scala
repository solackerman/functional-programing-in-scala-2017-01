package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(i, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  lazy val genHeapVals: Gen[(List[Int], H)] = for {
    l <- arbitrary[List[Int]]
  } yield (l.sorted, l.foldRight(empty)(insert))

  implicit lazy val arbHeapVals: Arbitrary[(List[Int], H)] = Arbitrary(genHeapVals)

  def removeAll(h: H): List[Int] =
    if (isEmpty(h)) Nil
    else findMin(h) :: removeAll(deleteMin(h))

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("sorted list in == sorted list out") = forAll { (x: (List[Int], H), y: (List[Int], H)) =>
    val (xl, xh) = x
    val (yl, yh) = y
    xl == removeAll(xh) && yl == removeAll(yh) && (xl ++ yl).sorted == removeAll(meld(xh, yh))
  }
}
