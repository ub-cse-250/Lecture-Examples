package cse250.examples.analysis

object FunctionAnalysis {
  def indexOf[T](seq: Seq[T], value: T, from: Int): Int = {
    for (i <- from until seq.length)
      if (seq(i) == value) return i
    -1
  }

  def count[T](seq: Seq[T], value: T): Int = {
    var count = 0;
    var i = indexOf(seq, value, 0)
    while (i != -1) {
      count += 1
      i = indexOf(seq, value, i + 1)
    }
    count
  }

  def binary_search(seq: collection.mutable.Seq[Int], value: Int): Int = {
    var begin = 0;
    var end = seq.length;
    while (begin != end) {
      val mid = begin + (end - begin) / 2
      println(mid)
      if (value < seq(mid)) end = mid
      else if (seq(mid) < value) begin = mid + 1
      else return mid
    }
    -1
  }

  def rec_binary_search(seq: collection.mutable.Seq[Int], value: Int): Int = {
    rec_binary_search(seq, value, 0, seq.length)
  }

  def rec_binary_search(seq: collection.mutable.Seq[Int], value: Int, begin: Int, end: Int): Int = {
    if (begin == end) -1
    else {
      val mid = begin + (end - begin) / 2
      if (value < seq(mid)) rec_binary_search(seq, value, begin, mid)
      else if (seq(mid) < value) rec_binary_search(seq, value, mid + 1, end)
      else mid
    }
  }

  // Contrast the generic functions below with the code above where the type is
  // hard-coded to Int. Since we do not know the type here, we add an argument
  // of comp that is either implicitly taken from the environment or explicitly
  // given upon execution.
  def binary_search[T](seq: collection.mutable.Seq[T], value: T)(implicit comp: Ordering[T]): Int = {
    var begin = 0;
    var end = seq.length;
    while (begin != end) {
      val mid = begin + (end - begin) / 2
      println(mid)
      if (comp.lt(value, seq(mid))) end = mid
      else if (comp.lt(seq(mid), value)) begin = mid + 1
      else return mid
    }
    -1
  }

  def rec_binary_search[T](seq: collection.mutable.Seq[T], value: T)(implicit comp: Ordering[T]): Int = {
    rec_binary_search(seq, value, 0, seq.length)
  }

  def rec_binary_search[T](seq: collection.mutable.Seq[T], value: T, begin: Int, end: Int)(implicit comp: Ordering[T]): Int = {
    if (begin == end) -1
    else {
      val mid = begin + (end - begin) / 2
      if (comp.lt(value, seq(mid))) rec_binary_search(seq, value, begin, mid)
      else if (comp.lt(seq(mid), value)) rec_binary_search(seq, value, mid + 1, end)
      else mid
    }
  }
}
