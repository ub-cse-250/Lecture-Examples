package cse250.examples.analysis

import scala.collection.mutable

object IntBubbleSorter extends Sorter[Int] {
  def sort(seq: mutable.Seq[Int]): Unit = {
      val n = seq.length
      for (i <- n-2 to 0 by -1; j <- 0 to i) {
        if (seq(j+1) < seq(j)) {
          val temp = seq(j+1)
          seq(j+1) = seq(j)
          seq(j) = temp
        }
      }
  }

  def sort(seq: Seq[Int]): Seq[Int] = {
    val newSeq = seq.toArray
    val n = seq.length
    for (i <- n-2 to 0 by -1; j <- 0 to i) {
      if (newSeq(j+1) < newSeq(j)) {
        val temp = newSeq(j+1);
        newSeq(j+1) = newSeq(j);
        newSeq(j) = temp
      }
    }
    newSeq.toList
  }
}
