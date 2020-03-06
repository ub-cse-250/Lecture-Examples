package cse250.examples.analysis

trait Sorter[T] {
  /** Perform in-place sorting of mutable input Seq. */
  def sort(seq: collection.mutable.Seq[T]): Unit

  /** Perform sorting and return copy of immutable Seq in sorted order. */
  def sort(seq: Seq[T]): Seq[T]
}
